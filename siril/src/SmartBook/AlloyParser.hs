{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SmartBook.AlloyParser
where

import qualified Data.ByteString                as BS
import qualified Data.Text.Lazy.IO              as TexL
import qualified Data.Text.Lazy                 as TexL
import qualified Data.Text.Lazy.Encoding        as TexL
import           Text.Parsec
import           Control.Monad                               (when)

import           SmartBook.Type

data Chp = Chp 
    { chpTitle :: Text
    , chpDesc :: Text
    , chpParagraphs :: [Text] 
    } deriving (Show, Eq)

empty = Chp { chpTitle = "", chpDesc = "", chpParagraphs = []}

-- Parsec errors are rather uninformative. The function extracts a portion of the input that has caused failure
readableError :: String -> Int -> Parsec Text st a
readableError msg maxInputLen = do
    s <- (stateInput <$> getParserState)
    unexpected . message . TexL.unpack $ s  -- <- lookAhead (count 15 anyChar)
    where message str
            | length str > maxInputLen = msg ++ "\"" ++ (take maxInputLen str) ++ " ..."
            | otherwise = msg ++ "\"" ++ str ++ "\""

tag :: String -> String -> Parsec Text st Text
tag tagOpen tagClose = do
      spaces 
      (try $ string tagOpen) <|> readableError "- failed to parse in 'tag' at " 30
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
    (desc, pars) <-  (try h2ppp) 
                 <|> (try ppph2ppp) 
                 <|> (try ppp) 
                 <|> readableError "- failed to parse in 'withTitle' at " 30
    let woBr = filter (not . TexL.null . TexL.strip) pars
    return $ Chp { chpTitle = title, chpDesc = desc, chpParagraphs = woBr }
    where paragraphs = manyTill (tag "<p>" "</p>") . try . lookAhead $ do
                spaces
                -- will parse until we failed to meet the starting <p>
                notFollowedBy $ string "<p>"
          h2ppp = do  -- <h2> followed by paragraphs
                desc <- tag "<h2>" "</h2>"
                pars <- paragraphs
                return (desc, pars)
          ppph2ppp = do -- empty paragraphs followed by <h2> followed by paragraphs
            emptyPars <- option [] $ manyTill (tag "<p>" "</p>") . try . lookAhead $ do
                        spaces
                        notFollowedBy $ string "<p>"
                        
            when (not . all (TexL.null . TexL.strip) $ emptyPars) $ 
                    unexpected "to fail the parser"
                    
            desc <- tag "<h2>" "</h2>"
            pars <- paragraphs
            return (desc, pars)
          ppp = do -- paragraphs only
            pars <- paragraphs
            return ("", pars)

paragraphsOnly :: Parsec Text st Chp
paragraphsOnly = do
    pars <- many1 $ tag "<p>" "</p>"
    -- the same as above. treat <br> as an empty row. 
    let woBr = filter (not . TexL.null . TexL.strip) pars
    return $ Chp { chpTitle = "", chpDesc = "", chpParagraphs = woBr }

parseChapter :: Parsec Text st Chp
parseChapter = (try withTitle) <|> paragraphsOnly

-- Input: text with <h1>, <h2> and <p> tag within
runBookParser :: Text -> Either ParseError [Chp]
runBookParser = 
    -- treat <br> as an empty row. 
    -- this is to allow the user to align the text 'return' in editors
    runParser parser () "Book input parser" . TexL.replace "<br>" "" 
    where parser = do
            val <- many $ do
                r <- parseChapter
                spaces
                return r
            eof
            return . filter (/= empty) $ val