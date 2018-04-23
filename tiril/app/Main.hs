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
import           Data.FileEmbed                             (embedStringFile)
import           Data.String
import           System.Environment                         (getExecutablePath)
import           System.FilePath                            (dropFileName, (</>))
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import           Control.Concurrent                          (forkIO)
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
    -- Including stuff from Foundation 6 + Dragula
    UI.addStyleSheet win "foundation.css"
    UI.addStyleSheet win "dragula.css"
    UI.addStyleSheet win "app.css"
    -- Removing flash artifacts. Suggested by Foundation CSS
    html <- head <$> getElementsByTagName win "html"
    element html #. "no-js"

    -- Adding menu
    mainMenu <- runMenu $ do 
        createTopBarMenu "Tiril"
        menu "Review"
        subMenu "Session" viewSession
    void $ getBody win #+ [element mainMenu]
                                        
    -- Including stuff from Foundation 6 + Dragula
    void $ getBody win #+ [ mkElement "script" # set UI.src "/static/js/vendor/what-input.js"
                          , mkElement "script" # set UI.src "/static/js/vendor/dragula.js"
                          , mkElement "script" # set UI.src "/static/js/vendor/foundation.js"
                          , mkElement "script" # set UI.src "/static/js/app.js" ]
    where viewSession = const $ do
            sessionWords <- liftIO $ do
                conn <- connectSqlite3 databaseName
                res <- (either (flip (:) [] . T.pack . show) id) <$> getWords conn
                HDBC.disconnect conn
                return res
            win <- askWindow
            void $ getBody win #+ [ UI.div #. "card-list dragula-container" 
                                           #+ (makeCard . T.unpack <$> sessionWords) ]
            
          makeCard :: String -> UI Element
          makeCard word = do
                card <- UI.div #. "card grow"
                               #+ [ UI.img #. "draggable" 
                                           # set UI.src "static/ico/drag.svg"
                                  , UI.a # set text word ]
                on UI.click card $ const $ runFunction $ ffi "selectCard(%1)" card
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

                return card