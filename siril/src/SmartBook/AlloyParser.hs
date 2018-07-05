{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SmartBook.AlloyParser
where

import qualified Data.ByteString                as BS
import qualified Data.Text.Lazy.IO              as TexL
import qualified Data.Text.Lazy                 as TexL
import qualified Data.Text.Lazy.Encoding        as TexL
import           Text.ParserCombinators.Parsec          (oneOf, manyTill, anyChar, string, char, eof, ParseError, spaces, lookAhead, option)
import           Text.Parsec.Prim                       (runParser, Parsec, (<?>), (<|>), many, skipMany, try, runP)
import           Text.Parsec.Combinator                 (many1, notFollowedBy)

import           SmartBook.Type

data Chp = Chp 
    { chpTitle :: Text
    , chpDesc :: Text
    , chpParagraphs :: [Text] 
    } deriving (Show, Eq)
    
tag :: String -> String -> Parsec Text st Text
tag tagOpen tagClose = do
      spaces
      string tagOpen
      (ret::String) <- manyTill anyChar $ 
          peek tagClose
      string tagClose
      return . TexL.pack $ ret
  where -- Doesn't consume upon failure and still doesn't consume upon success
        -- Allows to peek ahead safely without any data consumption
        peek :: String -> Parsec Text st String
        peek str = try . lookAhead $
                    string str

withTitle :: Parsec Text st Chp
withTitle = do
    title <- tag "<h1>" "</h1>"
    desc <- option "" . try $
        tag "<h2>" "</h2>"
    -- will parse until we failed to meet the starting <p>
    pars <- manyTill (tag "<p>" "</p>") . try . lookAhead $ do
        spaces
        notFollowedBy $ string "<p>"
    return $ Chp { chpTitle = title, chpDesc = desc, chpParagraphs = pars }
                    
paragraphsOnly :: Parsec Text st Chp
paragraphsOnly = do
    pars <- many1 $ tag "<p>" "</p>"
    return $ Chp { chpTitle = "", chpDesc = "", chpParagraphs = pars }

parseChapter :: Parsec Text st Chp
parseChapter = (try withTitle) <|> paragraphsOnly

-- Input: text with <h1>, <h2> and <p> tag within
runBookParser :: Text -> Either ParseError [Chp]
runBookParser = runParser parser () "Html Book parser"
    where parser = do
            val <- many $ do
                r <- parseChapter
                spaces
                return r
            return val