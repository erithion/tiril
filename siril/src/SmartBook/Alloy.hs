{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SmartBook.Alloy
where

import           GHC.Generics
import qualified Data.Text.Lazy                      as TL
import qualified Data.Text.Lazy.Encoding             as TL
import           System.FilePath                            (takeFileName)
import           Data.Map                                   (singleton, union)
import qualified Control.Arrow                       as Ar 
import           Text.ParserCombinators.Parsec.Error        (messageString, errorMessages)

import SmartBook.Sb
import SmartBook.Crypto
import SmartBook.AlloyParser

-- Default book config suitable for Alloy
defaultBook bookName bookAuthor fileName chapters = Book
    { _lang = "en"
    , _name = ""
    , _rusName = ""
    , _author = ""
    , _hash = ""
    , _thumbnail = ""
    , _filename = fileName
    , _size = 0
    , _langs = 
        [ Lang { name = bookName
               , lang = "en"
               , translation = Nothing
               , translationSize = Nothing
               , author = Just bookAuthor }
        , Lang { name = ""
               , lang = "ru"
               , translation = Just ""
               , translationSize = Just 0
               , author = Nothing } 
        ]
    , _chapters = chapters }


data AlloyData = AlloyData
    { bookTitle :: Text
    , bookAuthor :: Text
    , encryptResult :: Bool
    , bookLeft :: Text
    , bookRight :: Text
    } deriving (Generic, Show)

instance ToSmartBook AlloyData where
    sbJson file dat = Ar.left (show {- concat . map messageString $ errorMessages -}) $ do
        let title = bookTitle dat
        let author = bookAuthor dat
        left <- runBookParser . bookLeft $ dat
        right <- runBookParser . bookRight $ dat
        let name = TL.pack . takeFileName . TL.unpack $ file
        let chapters = zipWithDefault 
                            zipChapters 
                            (chapter "en" <$> left)
                            (chapter "ru" <$> right) 
                            defChapter defChapter
        return $ defaultBook title author name chapters
        where 
          defChapter = ChapterLeaf { chapterName = ""
                                   , chapterDescription = Just ""
                                   , paragraphs = [] }
          chapter :: Text -> Chp -> Content
          chapter lang c = ChapterLeaf { chapterName = chpTitle c
                                       , chapterDescription = Just $ chpDesc c
                                       , paragraphs = fmap (singleton lang) $ chpParagraphs c }
          zipWithDefault fn left right defLeft defRight = 
                let maxLen = max (length left) (length right)
                    padLeft = take (maxLen - length left) $ repeat defLeft
                    padRight = take (maxLen - length right) $ repeat defRight
                in zipWith fn (left ++ padLeft) (right ++ padRight)
            
          zipChapters :: Content -> Content -> Content
          zipChapters left right = 
                let parLeft = paragraphs left
                    parRight = paragraphs right
                    maxLen = max (length parLeft) (length parRight)
                    padLeft = take (maxLen - length padLeft) $ repeat (singleton "en" "")
                    padRight = take (maxLen - length padRight) $ repeat (singleton "ru" "")
                in left {paragraphs = zipWithDefault union (paragraphs left) (paragraphs right) (singleton "en" "") (singleton "ru" "") }