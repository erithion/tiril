{-# LANGUAGE OverloadedStrings #-}
module Type
where

import qualified Data.Text.Lazy as T

data TranslationEngine = Google | Lexin
    deriving (Show)

data TPS = Verb | Adverb | Adjective | Conjunction | Preposition | Interjection | Noun | Pronoun 
    deriving (Show)
    
-- TBD    
class Translator a where
    -- whence this translation came
    iam :: a -> TranslationEngine

