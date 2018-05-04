{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Session
where

import           Type
import           Data.Either
import           Database.HDBC
import qualified Data.Text.Lazy             as T

data SessionWord = SessionWord 
    { sessionWord :: T.Text
    , sessionWordLang :: T.Text 
    }

defaultSessionWord = SessionWord 
    { sessionWord = ""
    , sessionWordLang = "" }
    
instance NewWord SessionWord where             
    newWord = T.unpack . sessionWord
    newLang = T.unpack . sessionWordLang
    newAgent = const "VLC Media Player"

            
addWord :: IConnection c => c -> T.Text -> T.Text -> IO (Either SqlError ())
addWord conn word lang = handleSql (return . Left) (const (Right ()) <$> withTransaction conn send)
    where 
        send :: IConnection conn => conn -> IO ()
        send c = do 
                statement <- prepare c "INSERT INTO words (word, lang) VALUES (?)"
                execute statement [toSql word, toSql lang]
                commit conn


getWords :: IConnection c => c -> IO (Either SqlError [SessionWord])
getWords conn = handleSql (return . Left) (Right <$> get conn)
    where 
        get :: IConnection c => c -> IO [SessionWord]
        get conn = do 
            -- Beware of lazy quickQuery. With the current approach it would likely to read nothing 
            -- by the time you decide to use the data with the connection has already been closed
            map makeWord <$> quickQuery' conn "SELECT lang, word FROM words" []
        makeWord :: [SqlValue] -> SessionWord
        makeWord (sqlLang:sqlWord:[]) = SessionWord { sessionWord = fromSql sqlWord, sessionWordLang = fromSql sqlLang }
