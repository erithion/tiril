{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Session
where

import           Data.Either
import           Database.HDBC
import qualified Data.Text.Lazy             as T

addWord :: IConnection c => c -> T.Text ->  IO (Either SqlError ())
addWord conn word = handleSql (return . Left) (const (Right ()) <$> withTransaction conn (flip send word))
    where 
        send :: IConnection conn => conn -> T.Text -> IO ()
        send c word = do 
                statement <- prepare c "INSERT INTO session (word) VALUES (?)"
                execute statement [toSql word]
                commit conn


    