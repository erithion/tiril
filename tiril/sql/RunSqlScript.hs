{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment               (getArgs)
import System.IO

--import System.IO.Strict                 (hGetContents)
--import System.IO hiding                 (hGetContents)
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Parsec.Prim                 (runPT, ParsecT, try)
import Text.Parsec.Char                 (string, char, anyChar, newline)
import Text.Parsec.Combinator           (many1, manyTill, choice)
import Text.Parsec.Error                (ParseError)
import Control.Monad                    (when, guard)
import Data.Either                      (either)

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

main = 
    do 
        args <- getArgs
        when (length args /= 1) (putStrLn "You must supply a script file to run")
        guard (length args == 1)
        
        let fileName = head args

        conn <- connectSqlite3 "tiril.db"
        (script::Either ParseError [String]) <- withFile fileName ReadMode $ \handle->
            do
                val <- hGetContents handle
                runPT sqlScript () fileName val
        flip (either $ putStrLn . show) script $ \xs->
            do  -- sequence $ putStrLn <$> xs
                sequence $ flip map xs $ \x->
                      do -- Prepare of 'create index' fails if prepared 'create table' was not yet executed, thus we have to handle SQL command one by one :(
                         q <- prepare conn x
                         execute q []
                commit conn
               
                commit conn
        disconnect conn
            

        