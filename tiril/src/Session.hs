{-# LANGUAGE OverloadedStrings #-}
module Session
where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import qualified Data.Text.Lazy             as T

{- TODO
1. make a signature addWord :: IConnection c => c -> T.Text -> IO () and move out the connection
2. think of errors to return
-}
addWord :: T.Text -> IO ()
addWord word = do
            conn <- connectSqlite3 "F:\\git\\tiril\\tiril\\SQL\\tiril.db"
            statement <- prepare conn "INSERT INTO session (word) VALUES (?)"
            execute statement [toSql word]
            commit conn
            disconnect conn


    