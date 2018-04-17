{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GoogleTranslate
--    ( someFunc
--    ) 
where

import Type

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE

import           Text.ParserCombinators.Parsec        (oneOf, manyTill, anyChar, string, eof, ParseError)
import           Text.Parsec.Prim                     (runParser, Parsec, (<|>), (<?>), many, skipMany, runP)
import           Network.HTTP.Simple
import qualified Control.Arrow as Ar 

data Tir = Tir
    { sourceText :: T.Text
    , sourceLang :: T.Text
    , targetText :: Maybe T.Text
    , targetLang :: Maybe T.Text
    , targetTPS  :: Maybe TPS
    , targetIPA  :: Maybe T.Text
    }
    deriving (Show)

defaultTir = Tir    
    { sourceText = ""
    , sourceLang = ""
    , targetText = Nothing
    , targetLang = Nothing
    , targetTPS = Nothing
    , targetIPA = Nothing
    }

{-                           
[
   [
    [\"\1047\1076\1088\1072\1074\1089\1090\1074\1091\1081\1090\1077\",\"hello\",null,null,1]
   ]
   ,null,\"en\"
] 
-}   
tParser :: Parsec T.Text st [Tir]
tParser = 
    do string "[[["
       target <- manyTill anyChar (oneOf ",")
       src <- manyTill anyChar (oneOf ",")
       null1 <- manyTill anyChar (oneOf ",")
       null2 <- manyTill anyChar (oneOf ",")
       num <- manyTill anyChar (oneOf "]")
       string "],"
       null3 <- manyTill anyChar (oneOf ",")
       srcLang <- manyTill anyChar (oneOf "]")
       eof
       return . (:[]) $ defaultTir { sourceText = T.pack src, targetText = Just (T.pack target), sourceLang = T.pack srcLang }

runGoogleTParser :: T.Text -> Either ParseError [Tir]
runGoogleTParser = runParser tParser () "Google Translate dt=t parser"

googleTranslateWithT :: T.Text -> IO (Either T.Text [Tir])
googleTranslateWithT x =
    do
        req <- parseRequest $ ("https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=no&tl=ru&dt=t&q=" ++ T.unpack x)
        response <- httpLBS req

        putStrLn $ "The status code was: " ++
                    show (getResponseStatusCode response)
        print $ getResponseHeader "Content-Type" response
--        print $ getResponseBody response
        let (k :: Either ParseError [Tir]) = runGoogleTParser . TE.decodeUtf8 . getResponseBody $ response
        -- arrow over Left to transform ParseError into T.Text
        return . Ar.left (T.pack . show) $ k
--        return [T.pack "тест"]

