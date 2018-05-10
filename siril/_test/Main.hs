{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Hspec
import qualified Data.Text.Lazy                 as T
import           Lexin

main :: IO ()
main = hspec $ do
  describe "Lexin translate" $ do
    it "returns the expected output for the word \"hello\"" $
      (lexinTranslate . T.fromStrict $ "hello") `shouldReturn` 
        [[LexinWord {lexinWord = "hallo", lexinLang = "N", lexinType = "LEM"}
         ,LexinWord {lexinWord = "hallo, hello", lexinLang = "B", lexinType = "LEM"}
         ,LexinWord {lexinWord = "алло!", lexinLang = "RU", lexinType = "LEM"}
         ,LexinWord {lexinWord = "en hilsningsfrase i f eks en telefonsamtale eller som hilsen, hei", lexinLang = "N", lexinType = "DEF"}
         ,LexinWord {lexinWord = "a call (over the phone, loudspeaker, etc.) or greeting", lexinLang = "B", lexinType = "DEF"}]
        ,[LexinWord {lexinWord = "hallo, det er hos Hansen", lexinLang = "N", lexinType = "EKS"}
         ,LexinWord {lexinWord = "hello, Hansen (is) speaking", lexinLang = "B", lexinType = "EKS"}
         ,LexinWord {lexinWord = "алло! Хансен у телефона", lexinLang = "RU", lexinType = "EKS"}]
        ,[LexinWord {lexinWord = "hallo, er det noen der?", lexinLang = "N", lexinType = "EKS"}
         ,LexinWord {lexinWord = "hello, anybody there?", lexinLang = "B", lexinType = "EKS"}
         ,LexinWord {lexinWord = "алло! тут есть кто-нибудь", lexinLang = "RU", lexinType = "EKS"}]
        ,[LexinWord {lexinWord = "jeg stakk innom bare for å si hallo", lexinLang = "N", lexinType = "EKS"}
         ,LexinWord {lexinWord = "I dropped by just to say hello", lexinLang = "B", lexinType = "EKS"}
         ,LexinWord {lexinWord = "я зашёл просто поприветствовать", lexinLang = "RU", lexinType = "EKS"}]
        ,[LexinWord {lexinWord = "god dag", lexinLang = "N", lexinType = "LEM"}
         ,LexinWord {lexinWord = "good morning (good afternoon, good evening), hello", lexinLang = "B", lexinType = "LEM"}
         ,LexinWord {lexinWord = "добрый день", lexinLang = "RU", lexinType = "LEM"}
         ,LexinWord {lexinWord = "en hilsningsfrase; jeg ønsker deg en god dag, hei, hallo", lexinLang = "N", lexinType = "DEF"}
         ,LexinWord {lexinWord = "a greeting expression; have a nice day, hello", lexinLang = "B", lexinType = "DEF"}]
        ,[LexinWord {lexinWord = "hei", lexinLang = "N", lexinType = "LEM"}
         ,LexinWord {lexinWord = "hello!, hallo!, (hi US)", lexinLang = "B", lexinType = "LEM"}
         ,LexinWord {lexinWord = "привет", lexinLang = "RU", lexinType = "LEM"}
         ,LexinWord {lexinWord = "en hilsnings- og avskjedsfrase; hallo, god dag", lexinLang = "N", lexinType = "DEF"}
         ,LexinWord {lexinWord = "a greeting and parting expression", lexinLang = "B", lexinType = "DEF"}]
        ,[LexinWord {lexinWord = "hei på deg!", lexinLang = "N", lexinType = "EKS"}
         ,LexinWord {lexinWord = "hello!, (hi US)", lexinLang = "B", lexinType = "EKS"}
         ,LexinWord {lexinWord = "привет!", lexinLang = "RU", lexinType = "EKS"}]
        ,[LexinWord {lexinWord = "hei, lenge siden sist!", lexinLang = "N", lexinType = "EKS"}
         ,LexinWord {lexinWord = "hello, it's been a long time since the last time I saw you!", lexinLang = "B", lexinType = "EKS"}
         ,LexinWord {lexinWord = "привет, давно не виделись", lexinLang = "RU", lexinType = "EKS"}]
        ,[LexinWord {lexinWord = "hei, så lenge!", lexinLang = "N", lexinType = "EKS"}
         ,LexinWord {lexinWord = "bye-bye!", lexinLang = "B", lexinType = "EKS"}
         ,LexinWord {lexinWord = "пока, до встречи", lexinLang = "RU", lexinType = "EKS"}]
        ,[LexinWord {lexinWord = "heisan", lexinLang = "N", lexinType = "LEM"}
         ,LexinWord {lexinWord = "hello!, hallo!", lexinLang = "B", lexinType = "LEM"}
         ,LexinWord {lexinWord = "привет", lexinLang = "RU", lexinType = "LEM"}
         ,LexinWord {lexinWord = "en hilsningsfrase; hei, hallo", lexinLang = "N", lexinType = "DEF"}
         ,LexinWord {lexinWord = "a greeting expression; hello", lexinLang = "B", lexinType = "DEF"}]
        ,[LexinWord {lexinWord = "ohoi", lexinLang = "N", lexinType = "LEM"}
         ,LexinWord {lexinWord = "ahoy!", lexinLang = "B", lexinType = "LEM"}
         ,LexinWord {lexinWord = "эй!", lexinLang = "RU", lexinType = "LEM"}
         ,LexinWord {lexinWord = "(i tilrop:) hallo", lexinLang = "N", lexinType = "DEF"}
         ,LexinWord {lexinWord = "hello", lexinLang = "B", lexinType = "DEF"}]
        ,[LexinWord {lexinWord = "skip ohoi!", lexinLang = "N", lexinType = "EKS"}
         ,LexinWord {lexinWord = "ship ahoy!", lexinLang = "B", lexinType = "EKS"}
         ,LexinWord {lexinWord = "эй, на судне!", lexinLang = "RU", lexinType = "EKS"}]
        ,[LexinWord {lexinWord = "god kveld", lexinLang = "N", lexinType = "LEM"}
         ,LexinWord {lexinWord = "good evening", lexinLang = "B", lexinType = "LEM"}
         ,LexinWord {lexinWord = "en hilsningsfrase; jeg ønsker deg en god kveld, god aften", lexinLang = "N", lexinType = "DEF"}
         ,LexinWord {lexinWord = "a greeting expression; I wish you a good evening, hello", lexinLang = "B", lexinType = "DEF"}
         ,LexinWord {lexinWord = "greet", lexinLang = "B", lexinType = "LEM"}
         ,LexinWord {lexinWord = "здороваться", lexinLang = "RU", lexinType = "LEM"}
         ,LexinWord {lexinWord = "say hello [to]", lexinLang = "B", lexinType = "DEF"}]]

