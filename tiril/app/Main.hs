{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Database.HDBC.Sqlite3                      (connectSqlite3)
import           Database.HDBC                              (disconnect)

import           GoogleTranslate
import           Lexin
import           Session
import           Db
 
dbName =  "tiril.db" 

-- TODO: Find a better solution where to store SQL scripts and how to access them
main = do
    startOk <- dbExists dbName
    startOk <-  if not startOk then do
                    script <- readFile "F:/git/tiril/tiril/sql/create.sql"
                    res <- dbCreate dbName script
                    case res of 
                        Left v -> do 
                            putStrLn . show $ v
                            return False
                        _ -> return True
                else return True
    if startOk then do
        let port = 3000
        putStrLn $ "Listening on port " ++ show port
        run port app
    else return ()
 
app :: Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
app req respond = join $ respond <$>
    case pathInfo req of
        ("goo":[x]) -> do
                            tx <- googleTranslateWithT . T.fromStrict $ x
                            -- TODO: Think of a proper string type to avoid those packs and repacks
                            let xxs = either (return . T.pack . US.ushow) (return . T.pack . US.ushow) tx
                            index <$> xxs
        ("lex":[x]) -> do
                            tx <- lexinTranslate . T.fromStrict $ x
                            -- TODO: Currently you take merely 2 sections. Think of a proper extension to give out all available data
                            let (ttx :: [T.Text]) = take 2 . map (T.pack . US.ushow) $ tx
                            return $ index ttx
        ("add":[x]) -> do
                            conn <- connectSqlite3 dbName
                            r <- addWord conn . T.fromStrict $ x
                            disconnect conn
                            case r of
                                Left v -> return $ index ( "error" ++ show v)
                                Right _ -> return $ index "done"
 
 
index :: Show a => a -> Network.Wai.Response
index x = 
    responseBuilder status200 [("Content-Type", "text/html; charset=UTF-8")] $ 
        mconcat $ map copyByteString [ "<p>", BU.fromString .US.ushow  $ x, "</p>" ]

