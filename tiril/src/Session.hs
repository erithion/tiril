{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Session
where

import qualified Data.Text                      as TS
import           Control.Monad                            (void)
import           Control.Monad.Trans.Reader               (ReaderT)
import           Database.Persist.Sqlite                  (runSqlite, SqlBackend)
--import           Database.Persist
import           Database.Esqueleto

import           Type
import           Db

-- We don't need a separate type, we'll use the one that ORM has already made
-- Just make it an instance of the interface which Tiril knows how to deal with
instance NewWord Words where
    newWord = TS.unpack . wordsWord
    newLang = TS.unpack . wordsLang
    newAgent = const "VLC Media Player"
    
instance Translator (Words, Translations, Translators) where
    source = TS.unpack . wordsWord . (\(w, _, _) -> w)
    target = TS.unpack . translationsText . (\(_, t, _) -> t)
    lang = TS.unpack . translationsLang . (\(_, t, _) -> t) 
    -- part of speech
    verb = TS.unpack . translationsType_ . (\(_, t, _) -> t)
    -- whence this translation came
    iam = show . translatorsSrc . (\(_, _, s) -> s)

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
    
getTranslations :: TS.Text -> IO [(Words, Translations, Translators)]
getTranslations word = runSqlite "tiril.db" . asSqlBackendReader $ do 
    (ps::[(Entity Words, Entity Translations, Entity Translators)]) <- 
            select $ from $ \(wrd, tr, src) -> do
            where_ (wrd ^. WordsWord ==. val word &&. wrd ^. WordsId ==. tr ^. TranslationsWords_id &&. tr ^. TranslationsSrc_id ==. src ^. TranslatorsId)
            return (wrd, tr, src)
    return . map (\(x, y, z) -> (entityVal x, entityVal y, entityVal z)) $ ps