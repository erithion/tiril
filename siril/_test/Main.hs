{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Test.Hspec
import           Data.FileEmbed
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding        as TL
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString

import           Lexin
import SmartBook.Crypto
import SmartBook.Sb
import SmartBook.Alloy
import SmartBook.AlloyParser



enData :: Data.ByteString.ByteString
enData = $(embedStringFile  "_test/bkEn.txt")

noData :: Data.ByteString.ByteString
noData = $(embedStringFile  "_test/bkNo.txt")

sbData :: Data.ByteString.ByteString
sbData = $(embedStringFile  "_test/bkSb.txt")

main :: IO ()
main = hspec $ do
  lexinTests
  smartBookTests
  bookParserTests

lexinTests = do
  describe "Lexin translate" $ do
    it "returns the expected output for the word \"hello\"" $
      (lexinTranslate . TL.fromStrict $ "hello") `shouldReturn` 
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
         
smartBookTests = do
  describe "Smartbook.Crypto" $ do
    it "data == decrypt . encrypt $ data - padding needed" $
      (decrypt . TL.decodeUtf8 . BL.fromStrict. encrypt . TL.fromStrict $ "1234567890abcdef12") `shouldBe` Right "1234567890abcdef12"

    it "data == decrypt . encrypt $ data - padding NOT needed" $
      (decrypt . TL.decodeUtf8 . BL.fromStrict. encrypt . TL.fromStrict $ "1234567890abcdef") `shouldBe` Right "1234567890abcdef"

    it "data == decrypt . encrypt $ data - padding FAKE" $
      (decrypt . TL.decodeUtf8 . BL.fromStrict. encrypt . TL.fromStrict $ "1234567890abcde\3") `shouldBe` Right "1234567890abcde\3"
      
  describe "Smartbook.sbBinary" $ do
    let en = TL.decodeUtf8 . BL.fromStrict $ enData
    let no = TL.decodeUtf8 . BL.fromStrict $ noData
    let sb = Right  $ sbData
    let alloy = AlloyData { bookTitle = "Name"
                          , bookAuthor = "Author"
                          , encryptResult = False
                          , bookLeft = en
                          , bookRight = no }
                          
    it "simple book creation - NO non-ascii letters check" $
      (sbBinary "a9.sb" (encryptResult alloy) alloy) `shouldBe` sb
      
bookParserTests = do
  describe "Smartbook.InputAlloyHtml parser" $ do
    let empty = "";
    let emptyParagraph = "<p></p>";
    let emptyParagraph2 = "<p><br></p>";
    let chapterTitleOnly = "<h1></h1>";
    let chapterTitleOnly2 = "<h1><br></h1>";
    let chapterFullEmpty = "<h1></h1> <h2></h2>";
    let fullEmpty = "<h1></h1> <h2></h2> <p></p>";
    let fullNonEmpty = "<h1></h1> <h2></h2> <p>something</p>";
    let fullMultipleParagraphs = " <h1></h1> <h2></h2> <p>something</p> <p>something2</p>";
    let fullMultipleChapters = "<h1>1</h1> <h2>11</h2> <p>something</p> <p>something2</p>  \
                                 \ <h1>2</h1> <h2>22</h2> <p>something</p> <p>something2</p>";
    it "tested against empty input" $
      (runBookParser emptyParagraph) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = [""] } ] )
    it "tested against an sole empty paragraph" $
      (runBookParser emptyParagraph) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = [""] } ] )
    it "tested against <br> within a sole paragraph" $
      (runBookParser emptyParagraph2) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = ["<br>"] } ] )
    it "tested against a sole empty chapter title" $
      (runBookParser chapterTitleOnly) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = [] } ] )
    it "tested against a sole chapter title with <br> within" $
      (runBookParser chapterTitleOnly2) `shouldBe` 
            (Right $ [ Chp { chpTitle = "<br>", chpDesc = "", chpParagraphs = [] } ] )
    it "tested against a sole empty chapters title and description" $
      (runBookParser chapterFullEmpty) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = [] } ] )
    it "tested against a full but empty chapter" $
      (runBookParser fullEmpty) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = [""] } ] )
    it "tested against a full non empty chapter" $
      (runBookParser fullNonEmpty) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = ["something"] } ] )
    it "tested against a full chapter with multiple paragraphs" $
      (runBookParser fullMultipleParagraphs) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = ["something", "something2"] } ] )
    it "tested against multiple chapters with multiple paragraphs" $
      (runBookParser fullMultipleChapters) `shouldBe` 
            (Right $ [ Chp { chpTitle = "1", chpDesc = "11", chpParagraphs = ["something", "something2"] }
                     , Chp { chpTitle = "2", chpDesc = "22", chpParagraphs = ["something", "something2"] } ] )
