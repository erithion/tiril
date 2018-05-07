{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

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
import           Data.Tuple.Extra                            ((***), (&&&))
import           Data.Either
import           Control.Concurrent.Async                                       hiding (link)
import           Data.Hashable                               (hash)

import           Type
import           GoogleTranslate
import           Lexin
import           Session
import           Db
import           BootstrapMenu
import           Windows

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
        ("lex":[x]) -> do
            tx <- lexinTranslate . T.fromStrict $ x
            -- TODO: Currently you're taking merely 2 sections. Think of a proper extension to give out all available data
            return . index . take 2 $ tx
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
                # set UI.href "/static/app.css" ]
    -- Adding menu
    (mainMenu :: Element) <- evalMenu $ do
        navbar "Tiril"
        newDropdown "View"
        dropdownItem "Session" viewSession
        newDropdown "Export"
        dropdownItem "Anki" (const $ createMessageBlue "Anki" "Not yet, but it's coming ...")
        dropdownDivider
        dropdownItem "Memrise" (const $ createMessageGreen "Memrise" "Not yet, but it's coming also ...")
        search

    getBody win #+ [element mainMenu, createMainWindow]
                                        
    -- JS: Bootstrap + the rest
    getBody win 
        #+ [ mkElement "script" # set UI.src "/static/bootstrap.min.js"
           , mkElement "script" # set UI.src "/static/sortable.js"
           , mkElement "script" # set UI.src "/static/perfect-scrollbar.min.js"
           , mkElement "script" # set UI.src "/static/app.js" ]
           
    return ()
    where 
          jsInitSimpleScrollbar :: String -> UI ()
          jsInitSimpleScrollbar = runFunction . ffi "var ps = new PerfectScrollbar(%1)"
          jsToggleCard :: Element -> UI Int
          jsToggleCard = callFunction . ffi "uiToggleCard(%1)"
          jsUpdateSortable :: UI ()
          jsUpdateSortable = runFunction $ ffi "updateSortable()"
          jsInitApi = do
            add <- ffiExport jsAddTranslation
            delete <- ffiExport jsDeleteTranslation
            runFunction $ ffi "initApi(%1, %2)" add delete

          viewSession = const $ do
            sessionWords <- liftIO $ getWords
            clearMainWindow
            getMainWindow #+ [ UI.div #. "tiril-left-pane relative" #+ (makeCard <$> sessionWords) ]
            jsInitSimpleScrollbar ".tiril-left-pane"
          
          makeCard :: NewWord w => w -> UI Element
          makeCard word = do
            tr <- liftIO $ getTranslations . TS.pack . newWord $ word
            card <- UI.div 
                #+ [ UI.div 
                        #. "tiril-word rounded"
                        #+ [ mkElement "word" #. "m-1" # set text (newWord word)
                           , UI.span 
                                #. "badge badge-warning align-self-start" 
                                # set (attr "style") "font-size:50%;" # set text (newLang word) ]
                    , UI.ul #. "list" #+ (createWord 1 {- always 1 for now -}<$> tr) ]
                    
            on UI.click card $ const $ do
                deleteWindows "tiril-right-pane"
                isSelectedNow <- jsToggleCard card
                if isSelectedNow /= 0 then do
                    tr <- liftIO $ do
                        let googleTranslate word = do
                                res <- googleTranslateWithT (T.pack . newLang $ word) (T.pack "ru") (T.pack . newWord $ word)
                                -- TODO: Think of better ways to output the error
                                M.when (isLeft res) $ do
                                    -- We are not afraid of matching Left only,
                                    -- since we had the guard "when" before coming here
                                    let (Left x) = res
                                    -- and we just reporting the error on the screen
                                    putStrLn . T.unpack $ x
                                (return . either (const []) id $ res) 
                                    :: IO [Tir]

                        -- Run "stack bench" to see the results of async and sequential translation benchmarking
                        (uncurry concurrently . (googleTranslate &&& lexinTranslate . T.pack . newWord) $ word) 
                            :: IO ([Tir], [[LexinWord]])

                    -- We need a unique index for the each block, 
                    -- so the idea is to create a list of curry'ed functions first 
                    -- so that we could zip them with a list of indices later. So neat! I love Haskell!
                    let (fs :: [(Int->[UI Element])]) = uncurry (:) . (createWordPack *** (<$>) createWordPack) $ tr
                    let (ts :: [UI Element]) = concat $ zipWith ($) fs [1..]
                    void $ getMainWindow #+ [ UI.div #. "tiril-right-pane relative" 
                                                     #+ ts ]
                                                     
                    jsInitApi
                    jsUpdateSortable
                    jsInitSimpleScrollbar ".tiril-right-pane"
                else return ()
            return card
          
          createWordPack :: Translator a => [a] -> Int-> [UI Element]
          createWordPack ws idx = 
            [ mkElement "header" 
                #. "title"
            , UI.ul 
                #. "tiril-detail-container" 
                # set UI.id_ ("bar_" ++ show idx)
                #+ (createWord idx <$> ws) ]

          createWord :: Translator a => Int -> a -> UI Element
          createWord idx w = 
            UI.li 
                -- TODO: rework needed; too much data attributes seems to slow down the output
                #. "tiril-translation-word rounded"
                # set (attr "hash") (show . hash . target $ w)
                # set (attr "translator") (iam w)
                # set (attr "translation") (target w)
                # set (attr "lang") (lang w)
                # set (attr "verb") (verb w)
                #+ [ mkElement "word" 
                        # set text (target w) 
                   , mkElement "caps" 
                        #+  [ mkElement "cap" #. "mr-1 mb-1 badge badge-dark" # set (attr "style") "font-size:50%;" # set text (verb w)
                            , mkElement "cap" #. "mr-1 mb-1 badge badge-warning" # set (attr "style") "font-size:50%;" # set text (lang w)
                            , mkElement "cap" #. "mr-1 mb-1 badge badge-info" # set (attr "style") "font-size:50%;" # set text (iam w) ] 
                   , mkElement "i" 
                        #. "js-remove" 
                        # set (attr "data-parent-id") ("bar_" ++ show idx) 
                        # set text "✖" ]