{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Db 
--(DbError, dbCreate, dbExists)
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


import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite --(runSqlite, runMigration)
import Database.Persist.TH --(mkPersist, mkMigrate, persistLowerCase,
       --share, sqlSettings)
import Dictionary       

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

-----------------------------------------------------------------------
share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Words
   lang         Text
   word         Text
   deriving Show
Translations
   lang         Text
   text         Text
   hash         Text
   type_        Text
   words_id     WordsId  
   src_id       TranslatorsId
   deriving Show
Translators
   src          Dictionary
   href         Text
   deriving Show
|]

initDb :: IO ()
initDb = runSqlite "tiril.db" $ do -- runSqlite ":memory:" $ do
    -- runMigrationSilent migrateTables
    runMigration migrateTables 
    google <- insert $ Translators GoogleTranslate "https://translate.google.com"
    lexin <- insert $ Translators Lexin "http://lexin.udir.no"
    
    insert $ Words "no" "hello"
    insert $ Words "no" "sjefen"
    insert $ Words "no" "mener"
    insert $ Words "no" "snakke"
    insert $ Words "no" "ganger"
    insert $ Words "no" "vet"
    insert $ Words "no" "med"
    insert $ Words "en" "trust"
    insert $ Words "en" "utopia"
    insert $ Words "ga" "an bhó"
    insert $ Words "ga" "an mhaidin"
    insert $ Words "ga" "ceart go leor"
    insert $ Words "ga" "bus / i mbus / sa bhus"
    
    
    return ()