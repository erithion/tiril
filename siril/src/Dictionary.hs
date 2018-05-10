{-# LANGUAGE TemplateHaskell #-}
module Dictionary 
where

import Database.Persist.TH

data Dictionary = GoogleTranslate | Lexin
    deriving (Show, Read, Eq)
derivePersistField "Dictionary"
