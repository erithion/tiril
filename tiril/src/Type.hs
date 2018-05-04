{-# LANGUAGE OverloadedStrings #-}
module Type
where

import qualified Data.Text.Lazy as T

data TPS = Verb | Adverb | Adjective | Conjunction | Preposition | Interjection | Noun | Pronoun 
    deriving (Show)
    
class Translator a where
    source :: a -> String
    target :: a -> String
    lang :: a -> String
    -- part of speech
    verb :: a -> String
    -- whence this translation came
    iam :: a -> String
    
    
class NewWord a where
    newWord :: a -> String
    newLang :: a -> String
    newAgent :: a -> String

