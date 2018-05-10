{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Db 
where

import Data.Text                        (Text,      unpack)
import Database.Persist
import Database.Persist.Sqlite          (runSqlite, runMigration,     SqlBackend)
import Database.Persist.TH              (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Data.Hashable                    (hash)
import           Control.Monad.Trans.Reader               (ReaderT)
import           Data.Traversable                         (for)
import Control.Monad.IO.Class

import Dictionary       

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Words
   lang         Text
   word         Text
   deriving Show
Translations
   lang         Text
   text         Text
   hash         Int
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
initDb = runSqlite "tiril.db" $ do
    runMigration migrateTables 
    googleId <- insert $ Translators GoogleTranslate "https://translate.google.com"
    lexinId <- insert $ Translators Lexin "http://lexin.udir.no"
    -- TODO: temporary data for testing. Remove when unneeded
    insert $ Words "no" "hallo"
    id1 <- insert $ Words "no" "jeg trenger mer tid for å gjøre det på riktig måte og ikke ta noen feil"
    insert $ Words "uk" "хай йому трясця"
    insert $ Words "ga" "cén t-am ar tháinig tú abhaile inné"
    id2 <- insert $ Words "ga" "bhí mé ag ól an t-am go léir"
    insert $ Words "no" "ganger"
    insert $ Words "ga" "an bhó"
    insert $ Words "en" "Detectorists"
    insert $ Words "en" "Doctor Who"
    insert $ Words "no" "sjefen"
    insert $ Words "no" "snakke"
    insert $ Words "no" "mener"
    insert $ Words "ga" "bus / i mbus / sa bhus"
    insert $ Words "uk" "привіт"
    insert $ Words "ga" "an mhaidin"
    insert $ Words "ga" "ceart go leor"
    insert $ Words "no" "vet"
    insert $ Words "no" "med"
    insert $ Words "en" "trust"
{-
    insert $ Translations 
                "ru" 
                "Мне нужно больше времени, чтобы сделать это правильно и не допускать ошибок"
                (hash ("Мне нужно больше времени, чтобы сделать это правильно и не допускать ошибок" :: String))
                ""
                id1
                googleId

    insert $ Translations 
                "en" 
                "I was drinking all the time"
                (hash ("Were you drinking the whole time?" :: String))
                ""
                id2
                googleId

    insert $ Translations 
                "ru" 
                "я пил всё время"
                (hash ("Ты пил всё это время?" :: String))
                "EKS"
                id2
                googleId
-}                
    return ()
    
    
asSqlBackendReader2 :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader2 = id
    
addTranslation :: Text -> Text -> Text -> Text -> Dictionary -> IO ()
addTranslation word translation lang type_ src = runSqlite "tiril.db" . asSqlBackendReader2 $ do 

    (word_::Maybe (Entity Words)) <- selectFirst [WordsWord ==. word] []
    (src_::Maybe (Entity Translators)) <- selectFirst [TranslatorsSrc ==. src] []
    let (fields_::Maybe (Entity Words, Entity Translators)) = (,) <$> word_ <*> src_

    -- TODO: hash gives different values for String and Text. So reworkd and make finally a single type for all
    for fields_ $ \(Entity wid _, Entity sid _) ->
        insert_ $ Translations lang translation (hash . unpack $ translation) type_  wid sid
{-        
    liftIO $ do
        putStrLn . unpack $ translation
        putStrLn . show . hash $ translation
        putStrLn . show . hash . unpack $ translation
-}        
    return () 

deleteTranslation :: Text -> Int -> Dictionary -> IO ()
deleteTranslation word hash src = runSqlite "tiril.db" . asSqlBackendReader2 $ do 

    (word_::Maybe (Entity Words)) <- selectFirst [WordsWord ==. word] []
    (src_::Maybe (Entity Translators)) <- selectFirst [TranslatorsSrc ==. src] []
    let (fields_::Maybe (Entity Words, Entity Translators)) = (,) <$> word_ <*> src_

{- temporary    
    for fields_ $ \(Entity wid _, Entity sid _) -> do
        s <- selectFirst [TranslationsHash ==. hash, TranslationsWords_id ==. wid, TranslationsSrc_id ==. sid] []
        liftIO $ do
            putStrLn . show $ wid
            putStrLn . show $  sid
            putStrLn . show $  hash
            print s
-}
    
    for fields_ $ \(Entity wid _, Entity sid _) ->
        deleteWhere [TranslationsHash ==. hash, TranslationsWords_id ==. wid, TranslationsSrc_id ==. sid]
    return () 

jsAddTranslation word translation lang type_ src =
    case src of 
        "GoogleTranslate" -> addTranslation word translation lang type_ GoogleTranslate
        "Lexin" -> addTranslation word translation lang type_ Lexin
        x -> error $ "jsAddTranslation: src " ++ x ++ " is not defined"

jsDeleteTranslation word hash src =
    case src of 
        "GoogleTranslate" -> deleteTranslation word hash GoogleTranslate
        "Lexin" -> deleteTranslation word hash Lexin
        x -> error $ "jsDeleteTranslation: src " ++ x ++ " is not defined"