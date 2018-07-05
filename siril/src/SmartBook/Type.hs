{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SmartBook.Type
where

import GHC.Generics
import Data.Aeson.Types
import Data.List                                         ( drop, isInfixOf )
import Control.Applicative                               ( (<$>), (<*>), (<|>) )
import qualified Data.Aeson.Text            as Aeson 
import qualified Data.Text.Lazy.IO          as TexL
import qualified Data.Text.Lazy             as TexL
import qualified Data.Text.Lazy.Encoding    as TexL
import qualified Data.Map                   as Mp
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString            as BS

import           SmartBook.Crypto

type Text = TexL.Text

data Lang = Lang 
    { name :: Text
    , lang :: Text
    , translation :: Maybe Text
    , translationSize :: Maybe Int
    , author :: Maybe Text
    } deriving (Generic, Show, Eq)
        
data Book = Book 
    { _lang :: Text
    , _name :: Text
    , _rusName :: Text
    , _author :: Text
    , _hash :: Text
    , _thumbnail :: Text
    , _filename :: Text
    , _size :: Int
    , _langs :: [ Lang ]
    , _chapters :: [Content]
    } deriving (Generic, Show, Eq)
    
data Content 
    = ChapterTree {chapterName :: Text, chapterDescription :: Maybe Text, chapters :: [Content]}
    | ChapterLeaf {chapterName :: Text, chapterDescription :: Maybe Text, paragraphs :: [Mp.Map Text Text]}
    deriving (Generic, Show, Eq)
    
instance FromJSON Lang    
instance FromJSON Book where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance FromJSON Content where
    parseJSON = withObject "Content" $ 
            \v -> ChapterTree <$> v .: "chapterName" <*> v .:? "chapterDescription" <*> v .: "chapters" 
            <|>   ChapterLeaf <$> v .: "chapterName" <*> v .:? "chapterDescription" <*> v .: "paragraphs"
            <|>   fail "Content has neither `chapters` nor `paragraphs` tags"

instance ToJSON Lang where
   toJSON = genericToJSON defaultOptions {omitNothingFields = True}
instance ToJSON Book where
    toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue, fieldLabelModifier = drop 1}
instance ToJSON Content where
    toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue, omitNothingFields = True}

-- To produce SB books easier from various inputs such as html, fb2 or ePub
type Filename = Text
class ToSmartBook a where
    sbBinary :: Filename -> Bool -> a -> Either String BS.ByteString
    sbBinary file doEncryption input = let dat = Aeson.encodeToLazyText <$> (sbJSON file input)
                                       in if doEncryption  
                                          then encrypt <$> dat
                                          else BSL.toStrict . TexL.encodeUtf8 <$> dat    
    -- minimal def
    sbJSON :: Filename -> a -> Either String Book
                                          