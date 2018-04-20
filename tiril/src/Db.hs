{-# LANGUAGE ScopedTypeVariables #-}
module Db 
(DbError, dbCreate, dbExists)
where

import System.IO
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Parsec.Prim                 (runPT, ParsecT, try)
import Text.Parsec.Char                 (string, char, anyChar, newline)
import Text.Parsec.Combinator           (many1, manyTill, choice)
import Text.Parsec.Error                (ParseError)
import Data.Either                      (either)
import Data.Foldable

data Script = Comment String
            | EmptyLine
            | Sql String
            deriving (Show)
        
commentLine:: ParsecT String st IO Script
commentLine = string "--" >> manyTill anyChar newline >>= return . Comment
        
emptyLine :: ParsecT String st IO Script
emptyLine = many1 newline >> return EmptyLine
        
sqlOperator :: ParsecT String st IO Script
sqlOperator = manyTill anyChar (char ';') >>= return . Sql
        
sqlScript :: ParsecT String st IO [String]
sqlScript = 
    do  script <- many1 . choice $  
                    [ try commentLine
                    , try emptyLine 
                    , try sqlOperator ]
        return $ filter (not . null) $ map unwrap script
    where unwrap (Sql s) = s
          unwrap _ = ""

data DbError = DbSQLErr SqlError | DbParseErr ParseError deriving (Show)

-- TODO Produces SQL API misuse error, have no idea why yet
--      Make this function work and remove the one below
{- 
executeScript :: String -> String -> IO (Either DbError ())
executeScript dbName scriptText = do 
    (script::Either ParseError [String]) <- runPT sqlScript () "" scriptText
    conn <- connectSqlite3 dbName
    let ret = either (return . Left . DbParseErr) (\x-> handleSql (return . Left . DbSQLErr) (Right <$> runTransaction conn x)) script
    disconnect conn
    ret
    where 
        runTransaction :: IConnection c => c -> [String] -> IO ()
        runTransaction conn xs = withTransaction conn $ \_-> sequenceA_ $ flip map xs $ \x-> do
            q <- prepare conn x
            execute q []
-}

executeScript :: String -> String -> IO (Either DbError ())
executeScript dbName scriptText = do 
    conn <- connectSqlite3 dbName
    q <- prepare conn "DROP TABLE IF EXISTS session"
    execute q []
    q <- prepare conn "CREATE TABLE session (word TEXT UNIQUE ON CONFLICT IGNORE NOT NULL)"
    execute q []
    commit conn
    disconnect conn
    return $ Right ()

dbCreate :: String -> String -> IO (Either DbError ())
dbCreate name createScript = executeScript name createScript
            
-- Replace with getTables             
dbExists :: String -> IO Bool
dbExists dbName = do
    conn <- connectSqlite3 dbName
    (ret :: Bool) <- handleSql (const . return $ False) $ do
        (count :: Int) <- length <$> quickQuery conn "SELECT COUNT(*) FROM sqlite_master WHERE type='table'" []
        return $ count /= 0
    disconnect conn
    return ret

        