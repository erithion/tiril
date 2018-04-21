{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Session
where

import           Data.Either
import           Database.HDBC
import qualified Data.Text.Lazy             as T

addWord :: IConnection c => c -> T.Text -> IO (Either SqlError ())
addWord conn word = handleSql (return . Left) (const (Right ()) <$> withTransaction conn (flip send word))
    where 
        send :: IConnection conn => conn -> T.Text -> IO ()
        send c word = do 
                statement <- prepare c "INSERT INTO session (word) VALUES (?)"
                execute statement [toSql word]
                commit conn


getWords :: IConnection c => c -> IO (Either SqlError [T.Text])
getWords conn = handleSql (return . Left) (Right <$> get conn)
    where 
        get :: IConnection c => c -> IO [T.Text]
        get conn = do 
            -- Beware of lazy quickQuery. With the current approach it would likely to read nothing 
            -- by the time you decide to use the data with the connection has already been closed
            (concat . map (map fromSql)) <$> quickQuery' conn "SELECT word FROM session" []
