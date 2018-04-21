{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
module Main where

import qualified Data.Text                      as TS
import qualified Data.Text.Lazy                 as T
import qualified Data.ByteString.UTF8           as BU
import qualified Text.Show.Unicode              as US 
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types                         (status200)
import           Blaze.ByteString.Builder                   (copyByteString)
import           Control.Monad                                                  hiding (when)
import           Database.HDBC.Sqlite3                      (connectSqlite3)
import qualified Database.HDBC                  as HDBC     (disconnect)
import           Data.FileEmbed
import           Data.String
import           System.Environment                         (getExecutablePath)
import           System.FilePath                            (dropFileName, (</>))
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import           Control.Concurrent                          (forkIO)
import           Control.Monad.State
import           Data.IORef

import           GoogleTranslate
import           Lexin
import           Session
import           Db
import           Menu
 
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
        void $ forkIO $ startGUI config $ mainUi
        
        putStrLn $ "HTTP-server at http://localhost:" ++ show serverPort
        run serverPort mainServer
    else return ()
 
mainServer :: Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
mainServer req respond = join $ respond <$>
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

type Color = String
            
mainUi :: Window -> UI ()
mainUi win = do
    return win # set UI.title "Tiril"
    UI.addStyleSheet win "foundation.css"
    -- Removing flash artifacts. Suggested by Foundation CSS
    html <- head <$> getElementsByTagName win "html"
    element html #. "no-js"

    -- Including stuff from Foundation 6 + Dragula
    getHead win #+ [mkElement "link" # set (attr "rel" ) "stylesheet"
                                     # set (attr "type") "text/css"
                                     # set (attr "href") ("/static/css/app.css")
                   ,mkElement "link" # set (attr "rel" ) "stylesheet"
                                     # set (attr "type") "text/css"
                                     # set (attr "href") ("/static/css/dragula.css")]

    void $ getBody win #+ [runMenu $ createTopBarMenu "Tiril"
                                        >> menu "First" >> subMenu "Session B1" (sessionHandler "b1" "board1") >> subMenu "Session B2" (sessionHandler "b2" "board2")
                                        >> menu "Second" >> subMenu "Session B3" (sessionHandler "b3" "board3")]
                                        
    -- Including stuff from Foundation 6 + Dragula
    void $ getBody win #+ [ mkElement "script" # set (attr "src") ("/static/js/vendor/what-input.js")
                          , mkElement "script" # set (attr "src") ("/static/js/vendor/dragula.js")
                          , mkElement "script" # set (attr "src") ("/static/js/vendor/foundation.js")
                          , mkElement "script" # set (attr "src") ("/static/js/app.js")]
    where sessionHandler cardsId containerId = const $ do
            (vals :: [T.Text]) <- liftIO $ do
                conn <- connectSqlite3 databaseName
                res <- (either (flip (:) [] . T.pack . show) id) <$> getWords conn
                HDBC.disconnect conn
                return res
            let spans = T.unpack <$> vals
            win <- askWindow
            
            elDrop <- UI.div #. "cards container-width dragula-container"
                             # set (attr "id") cardsId

            let container elements = element elDrop #+ elements
            
            void $ getBody win #+ ((:[]) . container $ makeWord "green" <$> spans)
--            runFunction $ ffi "addDragula(%1)" (cardsId :: String)
            
          makeWord :: Color -> String -> UI Element
          makeWord color word = do
                a <- UI.a #. "cardtitle noselect"
                          # set text word
                elDrag <- UI.div #. "card word-font grow"
                                 #+ [UI.img #. "draggable" 
                                            # set (attr "src") "static/ico/drag.svg"
                                    , element a]
                on UI.click a $ const $ do
                    runFunction $ ffi "resetWordsSelection()"
                    element elDrag #. "card word-font selected grow"
{-                    
                    translations <- liftIO $ do
                        tx1 <- googleTranslateWithT . T.pack $ word
                        tx2 <- lexinTranslate . T.pack $ word
                        let goo = either (const "Google Translate error") US.ushow tx1
                        let lex = US.ushow $ tx2
                        return [goo, lex]
                    let spans = [UI.span #. "cardtitle noselect"
                                         # set text txt | txt <- translations]
                    elem <- UI.div #. "card"
                                    #+ spans
                    win <- askWindow
                    void $ getBody win #+ [element elem]-}

                return elDrag

            

index :: Show a => a -> Network.Wai.Response
index x = 
    responseBuilder status200 [("Content-Type", "text/html; charset=UTF-8")] $ 
        mconcat $ map copyByteString [ "<p>", BU.fromString . US.ushow $ x, "</p>" ]
