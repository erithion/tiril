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
import           Database.HDBC.Sqlite3                      (connectSqlite3)
import           Database.HDBC                              (disconnect)
import           Data.FileEmbed
import           Data.String

import           GoogleTranslate
import           Lexin
import           Session
import           Db
 
databaseName =  "tiril.db" 

schema :: IsString a => a
schema = $(embedStringFile "./app/schema.sql")

main = do
    startOk <- dbExists databaseName
    startOk <- if not startOk then do
                    res <- dbCreate databaseName schema
                    case res of 
                        Left v -> do 
                            putStrLn . show $ v
                            return False
                        _ -> dbExists databaseName
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
            return $ either index index tx
        ("lex":[x]) -> do
            tx <- lexinTranslate . T.fromStrict $ x
            -- TODO: Currently you take merely 2 sections. Think of a proper extension to give out all available data
            return . index . take 2 $ tx
        ("add":[x]) -> do
            conn <- connectSqlite3 databaseName
            r <- addWord conn . T.fromStrict $ x
            disconnect conn
            return $ either (index . show) (index . const "done") r
 
index :: Show a => a -> Network.Wai.Response
index x = 
    responseBuilder status200 [("Content-Type", "text/html; charset=UTF-8")] $ 
        mconcat $ map copyByteString [ "<p>", BU.fromString . US.ushow $ x, "</p>" ]
