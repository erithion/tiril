{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GoogleTranslate
where

import           Type
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T
import           Text.ParserCombinators.Parsec          (oneOf, manyTill, anyChar, string, char, eof, ParseError)
import           Text.Parsec.Prim                       (runParser, Parsec, (<|>), (<?>), many, skipMany, runP)
import           Network.HTTP.Simple
import qualified Control.Arrow                  as Ar 
import           System.CPUTime
import           Text.Printf


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

instance Translator Tir where
    source = T.unpack . sourceText
    target = (maybe "???" T.unpack) . targetText
    lang = (maybe "???" T.unpack) . targetLang
    verb = const "DND"
    iam = const "GoogleTranslate"

    
{-  Parsed pattern is                         
[
   [
    [\"\1047\1076\1088\1072\1074\1089\1090\1074\1091\1081\1090\1077\",\"hello\",null,null,1]
   ]
   ,null,\"en\"
] 
-}   
parserT :: Parsec T.Text st [Tir]
parserT = 
    do string "[[[\""
       target <- manyTill anyChar (oneOf "\"")
       string ",\""
       src <- manyTill anyChar (oneOf "\"")
       string ",null,null,"
       num <- manyTill anyChar (oneOf "]")
       string "],null,\""
       srcLang <- manyTill anyChar (oneOf "\"")
       char ']'
       eof
       return . (:[]) $ defaultTir { sourceText = T.pack src, targetText = Just (T.pack target), sourceLang = T.pack srcLang }

runGoogleTParser :: T.Text -> Either ParseError [Tir]
runGoogleTParser = runParser parserT () "Google Translate dt=t parser"

googleTranslateWithT :: T.Text -> T.Text -> T.Text -> IO (Either T.Text [Tir])
-- For some reason Ukrainian "ua" for Google is "uk"
googleTranslateWithT ("ua") to x = googleTranslateWithT "uk" to x
googleTranslateWithT from to x =
    do
        req <- parseRequest $ ("https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=" ++ T.unpack from ++ "&tl=" ++ T.unpack to ++ "&dt=t&q=" ++ T.unpack x)
        response <- httpLBS req
        let (k :: Either ParseError [Tir]) = runGoogleTParser . T.decodeUtf8 . getResponseBody $ response
        -- arrow over Left to transform ParseError into T.Text
        return . Ar.left (T.pack . show) $ k

        
        
{- TODO: Consider other types of translation requests, see below

https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&dt=t&q=hello
[[["Здравствуйте","hello",null,null,1]],null,"en"]

https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&dt=bd&q=hello
[null,[["глагол",["здороваться","звать","окликать"],[["здороваться",["greet","hello","salute","hallo","hullo","bow"],null,0.0028530264],["звать",["call","invite","shout","hail","hallo","hello"],null,2.7536449e-05],["окликать",["hail","holler","call","challenge","speak","hello"],null,2.7536449e-05]],"hello",2],["имя существительное",["приветствие","приветственный возглас","возглас удивления"],[["приветствие",["greeting","welcome","salute","salutation","hello","hail"],null,0.0014801305],["приветственный возглас",["hallo","halloa","hello","viva"],null,2.7536449e-05],["возглас удивления",["hallo","halloa","hello"],null,2.7536449e-05]],"hello",1]],"en"]

https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&dt=ex&q=hello
[null,null,"en",null,null,null,null,null,null,null,null,null,null,[[["I mean, it's nice in the way he wants to serve my sexual needs, but \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.005"],["Logan didn't say \u003cb\u003ehello\u003c/b\u003e , but I hadn't expected a greeting.",null,null,null,3,"m_en_us1254307.001"],["\u003cb\u003ehello\u003c/b\u003e, what's this?",null,null,null,3,"neid_9510"],["It was a pleasant surprise when Sheila Sheridan came over to say \u003cb\u003ehello\u003c/b\u003e .",null,null,null,3,"m_en_us1254307.001"],["She whispered \u003cb\u003ehello\u003c/b\u003e , then began to make her way to her room, where she hoped to take a nap.",null,null,null,3,"m_en_us1254307.001"],["\u003cb\u003ehello\u003c/b\u003e, what's all this then?",null,null,null,3,"m_en_us1254307.003"],["I mean - \u003cb\u003ehello\u003c/b\u003e - this is just kind of a witty, fun movie.",null,null,null,3,"m_en_us1254307.005"],["When their eyes met, she grinned wickedly in an informal \u003cb\u003ehello\u003c/b\u003e .",null,null,null,3,"m_en_us1254307.006"],["And \u003cb\u003ehello\u003c/b\u003e , what is this and why haven't I heard about it before?",null,null,null,3,"m_en_us1254307.003"],["He was a little surprised since he had already said \u003cb\u003ehello\u003c/b\u003e to her that morning.",null,null,null,3,"m_en_us1254307.001"],["We didn't get the chance to get together this visit, but we had nice phone conversation and a waved \u003cb\u003ehello\u003c/b\u003e .",null,null,null,3,"m_en_us1254307.006"],["My parents are chuckling - I guess more of my reaction than their news, but I mean, \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.005"],["Yeah, that might sound totally rude of me to say, but \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.005"],["Quentin is surprised to see Maggie, and says \u003cb\u003ehello\u003c/b\u003e .",null,null,null,3,"m_en_us1254307.001"],["Over the last two days I have been flooded with porn site IMs… \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.005"],["\u003cb\u003ehello\u003c/b\u003e! did you even get what the play was about?",null,null,null,3,"m_en_us1254307.005"],["I thought it summed up what I wanted to say and it also is a way to say \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.001"],["I mean, not that we have that all the time, coz \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.005"],["\u003cb\u003ehello\u003c/b\u003e there, Katie!",null,null,null,3,"m_en_gb0372340.001"],["You are supposed to be able to keep up with my voice, \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.005"],["I was stunned and I said I'm surprised anyone says \u003cb\u003ehello\u003c/b\u003e to me ever in the mall or in the store after reading that.",null,null,null,3,"m_en_us1254307.001"],["‘Oh, \u003cb\u003ehello\u003c/b\u003e ,’ she said acting surprised to see the four boys staring at her.",null,null,null,3,"m_en_us1254307.001"],["Every gathering she beds another partner who isn't her husband, \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.005"],["Umm… \u003cb\u003ehello\u003c/b\u003e , the world just ended, everyone seems bizarrely unaffected, like the predicted deep freeze has already reached their brains.",null,null,null,3,"m_en_us1254307.005"],["Ships' horns toot, children wave and call \u003cb\u003ehello\u003c/b\u003e , and every morning you're awakened by the haunting call of the muezzin from some distant village mosque.",null,null,null,3,"m_en_us1254307.001"],["So Bob, if you are out there, drop in and say \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.001"],["Like we have time for a life - \u003cb\u003ehello\u003c/b\u003e !",null,null,null,3,"m_en_us1254307.005"],["\u003cb\u003ehello\u003c/b\u003e there, Katie!",null,null,null,3,"m_en_us1254307.001"],["\u003cb\u003ehello\u003c/b\u003e, is anybody in?",null,null,null,3,"neid_9508"],["It is extraordinary how much can be achieved when you put enthusiasm into a routine task, a special project or a simple \u003cb\u003ehello\u003c/b\u003e or conversation.",null,null,null,3,"m_en_us1254307.006"]]]]

https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&dt=md&q=hello
[null,null,"en",null,null,null,null,null,null,null,null,null,[["имя существительное",[["an utterance of “hello”; a greeting.","m_en_us1254307.006"]],"hello"],["глагол",[["say or shout “hello”; greet someone.","m_en_us1254307.007"]],"hello"],["восклицание",[["used as a greeting or to begin a telephone conversation.","m_en_us1254307.001"]],"hello"]]]


https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&dt=rm&q=hello
[[[null,null,null,"heˈlō,həˈlō"]],null,"en",null,null,null,1,null,[["en"],null,[1],["en"]]]


https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&dt=ss&q=hello
[null,null,"en",null,null,null,null,null,null,null,null,[["имя существительное",[[["howdy","hullo","hi","how-do-you-do"],""]],"hello"],["восклицание",[[["hi","howdy","hey","hiya","ciao","aloha"],"m_en_us1254307.001"]],"hello"]]]


https://translate.googleapis.com/translate_a/single?client=gtx&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&dt=at&q=hello
[null,null,"en",null,null,[["hello",null,[["Здравствуйте",1000,true,false],["Привет",1000,true,false]],[[0,5]],"hello",0,0]]]


https://translate.google.com/#en/ru/hello
https://translate.googleapis.com/translate_a/single?client=t&ie=UTF-8&oe=UTF-8&dt=bd&dt=ex&dt=ld&dt=md&dt=qca&dt=rw&dt=rm&dt=ss&dt=t&dt=at&tk=token&sl=$en&tl=$ru&hl=$ru&q=hello

-}