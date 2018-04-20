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

-- Gathering SQL and Parser errors into a single type. 
-- See executeScript
data DbError = DbSQLErr SqlError 
             | DbParseErr ParseError 
             deriving (Show)

data Script = Comment String
            | EmptyLine
            | Sql String
            deriving (Show)
            
executeScript :: String -> String -> IO (Either DbError ())
executeScript dbName scriptText = do 
    (script::Either ParseError [String]) <- runPT sqlScript () "" scriptText
    conn <- connectSqlite3 dbName
    (ret :: Either DbError ()) <- either (return . Left . DbParseErr) (runScript conn) script 
    disconnect conn
    return ret
    where runScript :: IConnection c => c -> [String] -> IO (Either DbError ())
          runScript conn xs = handleSql (return . Left . DbSQLErr) . (<$>) Right . withTransaction conn . doTransaction $ xs
          
          doTransaction :: IConnection c => [String] -> c -> IO ()
          doTransaction xs conn = sequenceA_ $ flip map xs $ \x-> do
                q <- prepare conn x
                execute q []
                putStrLn $ "SQL executed: " ++ x
          -- Parser
          commentLine:: ParsecT String st IO Script
          commentLine = string "--" >> manyTill anyChar newline >>= return . Comment
        
          emptyLine :: ParsecT String st IO Script
          emptyLine = many1 newline >> return EmptyLine
        
          sqlOperator :: ParsecT String st IO Script
          sqlOperator = manyTill anyChar (char ';') >>= return . Sql

          unwrap (Sql s) = s
          unwrap _ = ""
          
          sqlScript :: ParsecT String st IO [String]
          sqlScript = do
            script <- many1 . choice $  
                [ try commentLine
                , try emptyLine 
                , try sqlOperator ]
            return $ filter (not . null) $ map unwrap script
                
dbCreate = executeScript
            
dbExists :: String -> IO Bool
dbExists dbName = do
    conn <- connectSqlite3 dbName
    count <- length <$> getTables conn
    disconnect conn
    putStrLn $ "DB tables: " ++ show count
    return $ count /= 0

        