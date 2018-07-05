{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text.Lazy                 as T
import qualified Data.Text                      as TS
import qualified Data.ByteString.UTF8           as BU
import qualified Text.Show.Unicode              as US 
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types                         (status200)
import           Blaze.ByteString.Builder                   (copyByteString)
import           Control.Monad                                                  hiding (when)
import qualified Control.Monad                  as M        (when)
import           System.Environment                         (getExecutablePath)
import           System.FilePath                            (dropFileName, (</>))
import           System.Directory                           (doesFileExist)
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import           Control.Concurrent                          (forkIO)

import           Type
import           GoogleTranslate
import           Lexin
import           Session
import           Db
import           BootstrapMenu
import           Windows
import           ExportMemrise
import           View
import           BuildSmartbook

databaseName =  "tiril.db" 

serverPort = 3000
uiPort = 3001

main = do
    exeDir <- dropFileName <$> getExecutablePath
    let dbPath = exeDir </> databaseName
    putStrLn $ "DB path " ++ dbPath
    startOk <- doesFileExist dbPath
    M.when (not startOk) $ initDb 
        
    putStrLn $ "AppUI at http://localhost:" ++ show uiPort
    let static = exeDir </> "static"
    putStrLn $ "Static dir " ++ show static
    let config = defaultConfig
                { jsPort   = Just uiPort
                , jsStatic = Just static
--                , jsLog = const (return ()) 
                }
    void $ forkIO $ startGUI config $ uiSetup
    
    putStrLn $ "HTTP-server at http://localhost:" ++ show serverPort
    run serverPort httpServer
 
httpServer :: Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
httpServer req respond = join $ respond <$>
    case pathInfo req of
        ("goo":from:to:w:xs) -> do
            tx <- googleTranslateWithT (T.fromStrict from) (T.fromStrict to) (T.fromStrict w)
            return $ either index index tx
        ("lex":"no":"ru":[x]) -> do
            tx <- lexinTranslate . T.fromStrict $ x
            -- TODO: Currently you're taking merely 2 sections. Think of a proper extension to give out all available data
            return . index . take 2 $ tx
        -- TODO: Add patterns for all languages Lexin supports
        ("lex":_) -> do
            return . index $ ""
        ("add":wd:lng:[]) -> do
            addWord wd lng
            return . index $ "done"
        _ -> return . index $ "Unknown command"
    where index :: Show a => a -> Network.Wai.Response
          index x = 
            responseBuilder status200 [ ("Content-Type", "text/html; charset=UTF-8") ] $ 
            mconcat $ map copyByteString [ "<p>", BU.fromString . US.ushow $ x, "</p>" ]
uiSetup :: Window -> UI ()
uiSetup win = do
    return win # set UI.title "Tiril"
    -- CSS: Bootstrap + the rest
    getHead win 
        #+ [ UI.meta 
                # set UI.name "viewport"
                # set UI.content "width=device-width, initial-scale=1, shrink-to-fit=no"
           , UI.link 
                # set UI.rel "stylesheet"
                # set UI.href "/static/bootstrap.min.css"
           , UI.link 
                # set UI.rel "stylesheet"
                # set UI.href "/static/perfect-scrollbar.css"
           , UI.link 
                # set UI.rel "stylesheet"
                # set UI.href "/static/assets/alloy-editor-ocean-min.css"
           -- As a static component AlloyEditor seems to have problems with its loading more than once, so it has been moved to global
--           , mkElement "script" # set UI.src "/static/alloy-editor-all.js"
           , mkElement "script" # set UI.src "/static/alloy-editor-all-min.js"
           , mkElement "script" # set UI.src "/static/alloy.jsx.js"
           ]
                
    -- Adding menu
    (mainMenu :: Element) <- evalMenu $ do
        navbar "Tiril"
        newDropdown "Words"
        dropdownItem "Latest" viewSession
        newDropdown "Export"
        dropdownItem "Anki" (const $ createMessageBlue "Anki" "Not yet, but it's coming ...")
        dropdownDivider
        dropdownItem "Memrise" (const $ memriseExportWindow)
        newDropdown "Tools"
        dropdownNamedDivider "SmartBook"
        dropdownItem "New" (const $ buildBook)
        dropdownItem "Encrypt ..." (const $ createMessageBlue "SB" "Very soon")
        dropdownItem "Decrypt ..." (const $ createMessageBlue "SB" "Very soon")
        search
{- uncomment after recording smartbook video        
    (mainMenu :: Element) <- evalMenu $ do
        navbar "Tiril"
        newDropdown "Dictionaries"
        dropdownItem "Latest words" viewSession
        dropdownNamedDivider "Export to ..."
        dropdownItem "Anki" (const $ createMessageBlue "Anki" "Not yet, but it's coming ...")
        dropdownItem "Memrise" (const $ memriseExportWindow)
        newDropdown "Tools"
        dropdownNamedDivider "SmartBook"
        dropdownItem "New" (const $ buildBook)
        dropdownItem "De-/encrypt" (const $ createMessageBlue "SB" "Very soon")
        search
-}
    getBody win #+ [element mainMenu, createMainWindow]

    -- JS: Bootstrap + the rest
    getBody win 
        #+ [ mkElement "script" # set UI.src "/static/bootstrap.min.js"
           , mkElement "script" # set UI.src "/static/sortable.js"
           , mkElement "script" # set UI.src "/static/perfect-scrollbar.min.js"
           , mkElement "script" # set UI.src "/static/common.js"
           ]
    return ()
