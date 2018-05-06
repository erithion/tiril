{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Session
where

import qualified Data.Text                      as TS
import           Control.Monad                            (void)
import           Control.Monad.Trans.Reader               (ReaderT)
import           Database.Persist.Sqlite                  (runSqlite, SqlBackend)
import           Database.Esqueleto

import           Type
import           Db

-- We don't need a separate type, we'll use the one that ORM has already made
-- Just make it an instance of the interface which Tiril knows how to deal with
instance NewWord Words where
    newWord = TS.unpack . wordsWord
    newLang = TS.unpack . wordsLang
    newAgent = const "VLC Media Player"

asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = id
    
addWord :: TS.Text -> TS.Text -> IO ()
addWord word lang = runSqlite "tiril.db" . asSqlBackendReader $ do
    void $ insert $ Words lang word
    
getWords :: IO [Words]
getWords = runSqlite "tiril.db" . asSqlBackendReader $ do 
    (ps::[Entity Words]) <- select $
                            from $ \p ->
                                  return p
    return . map entityVal $ ps
    
