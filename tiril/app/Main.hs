{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Main where

import qualified Data.Text.Lazy                 as T
import qualified Data.ByteString.UTF8           as BU
import qualified Text.Show.Unicode              as US 
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types                         (status200)
import           Blaze.ByteString.Builder                   (copyByteString)
import           Control.Monad                                                  hiding (when)
import qualified Control.Monad                  as M        (when)
import           Database.HDBC.Sqlite3                      (connectSqlite3)
import qualified Database.HDBC                  as HDBC     (disconnect)
import           Data.FileEmbed                             (embedStringFile)
import           Data.String                                (IsString)
import           System.Environment                         (getExecutablePath)
import           System.FilePath                            (dropFileName, (</>))
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import           Control.Concurrent                          (forkIO)
import           Data.Tuple.Extra                            ((***), (&&&))
import           Data.Either
import           Control.Concurrent.Async                    hiding(link)-- (concurrently, Concurrently, runConcurrently)

import           Type
import           GoogleTranslate
import           Lexin
import           Session
import           Db
import           BootstrapMenu
 
databaseName =  "tiril.db" 

schema :: IsString a => a
schema = $(embedStringFile "./app/schema.sql")

serverPort = 3000
uiPort = 3001

main = do
    exeDir <- dropFileName <$> getExecutablePath
    let dbPath = exeDir </> databaseName
    putStrLn $ "DB path " ++ dbPath
    startOk <- dbExists dbPath
    startOk <- if not startOk then do
                    res <- dbCreate dbPath schema
                    case res of 
                        Left v -> do 
                            putStrLn . show $ v
                            return False
                        _ -> dbExists dbPath
               else return True
    if startOk then do
        putStrLn $ "AppUI at http://localhost:" ++ show uiPort
        let static = exeDir </> "static"
        putStrLn $ "Static dir " ++ show static
        let config = defaultConfig
                    { jsPort   = Just uiPort
                    , jsStatic = Just static
--                    , jsLog = const (return ()) 
                    }
        void $ forkIO $ startGUI config $ uiSetup
        
        putStrLn $ "HTTP-server at http://localhost:" ++ show serverPort
        run serverPort httpServer
    else return ()
 
httpServer :: Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
httpServer req respond = join $ respond <$>
    case pathInfo req of
        ("goo":[x]) -> do
            tx <- googleTranslateWithT . T.fromStrict $ x
            return $ either index index tx
        ("lex":[x]) -> do
            tx <- lexinTranslate . T.fromStrict $ x
            -- TODO: Currently you take merely 2 sections. Think of a proper extension to give out all available data
            return . index . take 2 $ tx
        ("add":[x]) -> do
            conn <- connectSqlite3 databaseName
            r <- addWord conn . T.fromStrict $ x
            HDBC.disconnect conn
            return $ either (index . show) (index . const "done") r
        _ -> return . index $ "Unknown command"
    where index :: Show a => a -> Network.Wai.Response
          index x = 
            responseBuilder status200 [ ("Content-Type", "text/html; charset=UTF-8") ] $ 
            mconcat $ map copyByteString [ "<p>", BU.fromString . US.ushow $ x, "</p>" ]
            
uiSetup :: Window -> UI ()
uiSetup win = do
    return win # set UI.title "Tiril"
    -- CSS: Bootstrap + Sortable + own
    void $ getHead win #+ [ UI.meta # set UI.name "viewport"
                                    # set UI.content "width=device-width, initial-scale=1, shrink-to-fit=no"
                          , UI.link # set UI.rel "stylesheet"
                                    # set UI.href "/static/bootstrap.min.css"
                          , UI.link # set UI.rel "stylesheet"
                                    # set UI.href "/static/app.css"
                                    ]
    -- Adding menu
    (mainMenu :: Element) <- evalMenu $ do
        navbar "Tiril"
        newDropdown "View"
        dropdownItem "Session" viewSession
        dropdownDivider
        dropdownItem "DD3 Test2" (empty "DD3 T2")
        link "TEst 1" "#"
        newDropdown "Export"
        dropdownItem "Anki" (empty "Anki!? Not yet, but it's coming ...")
        dropdownDivider
        dropdownItem "Memrise" (empty "Memrise!? Not yet, but it's coming also ...")
        search
        link "TEst 2" "#"
        newDropdown "drop 3"
        dropdownItem "DD Test1" (empty "DD T1")
        dropdownDivider
        dropdownItem "DD Test2" (empty "DD T2")

    void $ getBody win #+ [element mainMenu, createMainWindow]
                                        
    -- JS: Bootstrap + Sortable + own
    void $ getBody win #+ [ mkElement "script" # set UI.src "/static/bootstrap.min.js"
                          , mkElement "script" # set UI.src "/static/sortable.js"
                          , mkElement "script" # set UI.src "/static/app.js" ]
    where createMainWindow :: UI Element
          createMainWindow = UI.div #. "tiril-main-window"

          getWindows :: String -> UI [Element]
          getWindows className = flip getElementsByClassName className =<< askWindow

          deleteWindows :: String -> UI ()
          deleteWindows className =
            void . sequence =<< map delete <$> getWindows className
          
          -- The window must exist already
          getMainWindow :: UI Element
          getMainWindow = head <$> getWindows "tiril-main-window"
          
          clearMainWindow = getMainWindow # set children []
          
          empty :: String -> () -> UI Element
          empty msg = const $ do
            clearMainWindow
            getMainWindow #+ [ UI.div # set text msg ]
            
          viewSession = const $ do
            sessionWords <- liftIO $ do
                conn <- connectSqlite3 databaseName
                res <- (either (flip (:) [] . T.pack . show) id) <$> getWords conn
                HDBC.disconnect conn
                return res
            clearMainWindow
            void $ getMainWindow #+ [ UI.div #. "tiril-word-list scrollbar-style-1" # set (attr "style") "display: flex; flex-flow: column nowrap;"
                                             #+ (makeCard . T.unpack <$> sessionWords) ]

          jsToggleCard :: Element -> UI Int
          jsToggleCard = callFunction . ffi "uiToggleCard(%1)"
          jsUpdateSortable :: UI ()
          jsUpdateSortable = runFunction $ ffi "updateSortable()"
          
          makeCard :: String -> UI Element
          makeCard word = do
                card <- UI.div
                               #+ [ UI.div #. "tiril-word rounded" # set text word
                                  , UI.ul #. "list" ]
                               
                on UI.click card $ const $ do
                    deleteWindows "tiril-right-pane"
                    isSelectedNow <- jsToggleCard card
                    if isSelectedNow /= 0 then do
                        tr <- liftIO $ do
                            let googleTranslate word = do
                                    res <- googleTranslateWithT word
                                    -- TODO: Think of better ways to output the error
                                    M.when (isLeft res) $ do
                                        -- We are not afraid of matching Left only,
                                        -- since we had the guard "when" before coming here
                                        let (Left x) = res
                                        -- and we just reporting the error on the screen
                                        putStrLn . T.unpack $ x
                                    let (ret :: [Tir]) = either (const []) id res
                                    return ret

                            -- Run "stack bench" to see the results of async and sequential translation benchmarking
                            (lex, goo) <- 
                                uncurry concurrently . (lexinTranslate &&& googleTranslate) . T.pack $ word

                            return (goo, lex)
                        -- We need a unique index for the each block, 
                        -- so the idea is to create a list of curry'ed functions first 
                        -- so that we could zip them with a list of indices later. So neat! I love Haskell!
                        let (fs::[(Int->[UI Element])]) = uncurry (:) . (createWordPack *** (<$>) createWordPack) $ tr
                        let (ts::[UI Element]) = concat $ zipWith ($) fs [1..]
                        void $ getMainWindow #+ [ UI.div #. "tiril-right-pane scrollbar-style-1" 
                                                         #+ ts ]
                        jsUpdateSortable
                    else return ()
                return card
          
          createWordPack :: Translator a => [a] -> Int-> [UI Element]
          createWordPack ws idx = [ mkElement "header" #. "title"
                                  , UI.ul #. "tiril-detail-container" 
                                          # set UI.id_ ("bar_" ++ show idx)
                                          #+ (createWord idx <$> ws)
                                  ]

          createWord :: Translator a => Int -> a -> UI Element
          createWord idx w = UI.li #. "tiril-translation-word rounded"
                                   #+ [ UI.span # set text (target w) 
                                                , mkElement "caps" #+
                                                        [mkElement "cap" #. "rounded" # set text (verb w)
                                                        ,mkElement "cap" #. "rounded" # set text (lang w)
                                                        ,mkElement "cap" #. "rounded" # set text (iam w) ] 
                                                , mkElement "i" #. "js-remove" # set (attr "data-parent-id") ("bar_" ++ show idx) # set text "✖"
                                      ]