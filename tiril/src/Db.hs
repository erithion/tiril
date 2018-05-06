{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Db 
where

import Data.Text                        (Text)
import Database.Persist
import Database.Persist.Sqlite          (runSqlite, runMigration)
import Database.Persist.TH              (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Dictionary       

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
initDb = runSqlite "tiril.db" $ do
    runMigration migrateTables 
    google <- insert $ Translators GoogleTranslate "https://translate.google.com"
    lexin <- insert $ Translators Lexin "http://lexin.udir.no"
    -- TODO: temporary data for testing. Remove when unneeded
    insert $ Words "no" "hallo"
    insert $ Words "no" "sjefen"
    insert $ Words "no" "mener"
    insert $ Words "no" "snakke"
    insert $ Words "no" "ganger"
    insert $ Words "no" "vet"
    insert $ Words "no" "med"
    insert $ Words "en" "trust"
    insert $ Words "en" "utopia"
    insert $ Words "en" "Detectorists"
    insert $ Words "en" "Jessica Hyde"
    insert $ Words "ga" "an bhó"
    insert $ Words "ga" "an mhaidin"
    insert $ Words "ga" "ceart go leor"
    insert $ Words "ga" "bus / i mbus / sa bhus"
    insert $ Words "ua" "привіт"
    insert $ Words "ua" "Хай йому трясця"
    
    return ()