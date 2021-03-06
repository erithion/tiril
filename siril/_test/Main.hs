{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Test.Hspec
import           Data.FileEmbed
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.Encoding        as TL
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString
import qualified Data.Map                       as Mp
import qualified Control.Arrow                  as Ar
import           Data.Either.Extra                            (fromLeft) 

import           Lexin
import SmartBook.Crypto
import SmartBook.Type
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
  bookParserEmptyTests
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
  describe "SmartBook.Crypto" $ do
    it "data == decrypt . encrypt $ data - padding needed" $
      (decrypt . TL.decodeUtf8 . BL.fromStrict. encrypt . TL.fromStrict $ "1234567890abcdef12") `shouldBe` Right "1234567890abcdef12"

    it "data == decrypt . encrypt $ data - padding NOT needed" $
      (decrypt . TL.decodeUtf8 . BL.fromStrict. encrypt . TL.fromStrict $ "1234567890abcdef") `shouldBe` Right "1234567890abcdef"

    it "data == decrypt . encrypt $ data - padding FAKE" $
      (decrypt . TL.decodeUtf8 . BL.fromStrict. encrypt . TL.fromStrict $ "1234567890abcde\3") `shouldBe` Right "1234567890abcde\3"
      
  describe "SmartBook.sbBinary" $ do
    let en = TL.decodeUtf8 . BL.fromStrict $ enData
    let no = TL.decodeUtf8 . BL.fromStrict $ noData
    let sb = Right sbData
    let alloy = defaultAlloy "Name" "Author" en no
                          
    it "simple book creation - NO non-ascii letters check" $
      (sbBinary "a9.sb" False alloy) `shouldBe` sb
      
  describe "SmartBook.sbJSON" $ do
    let en = "<h1>Chapter 1</h1> <h2>Sub chapter 1</h2> <p>something</p> <p>something2</p>  \
            \ <h1>Chapter 2</h1> <h2>Sub chapter 2</h2> <p>something</p> <p>something2</p>"
    let noLessChapters = "<h1>Chapter 1</h1> <h2>Sub chapter 1</h2> <p>something</p> <p>something2</p>"
    let noMoreChapters = "<h1>Chapter 1</h1> <h2>Sub chapter 1</h2> <p>something</p> <p>something2</p>  \
                        \ <h1>Chapter 2</h1> <h2>Sub chapter 2</h2> <p>something</p> <p>something2</p> \
                        \ <h1>Chapter 3</h1> <h2>Sub chapter 3</h2> <p>something</p> <p>something2</p>"
    let noLessPars = "<h1>Chapter 1</h1> <h2>Sub chapter 1</h2> <p>something</p>  \
                    \ <h1>Chapter 2</h1> <h2>Sub chapter 2</h2> <p>something</p> <p>something2</p>"
    let noMorePars = "<h1>Chapter 1</h1> <h2>Sub chapter 1</h2> <p>something</p> <p>something2</p> <p>something3</p>\
                    \ <h1>Chapter 2</h1> <h2>Sub chapter 2</h2> <p>something</p> <p>something2</p>"
    
    
    it "tested against 2 Chapters vs. 1 Chapter" $
        (sbJSON "" $ defaultAlloy "" "" en noLessChapters) `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" ".sb" $
                [ ChapterLeaf { chapterName = "Chapter 1"
                              , chapterDescription = Just "Sub chapter 1"
                              , paragraphs = [ Mp.fromList [("en", "something"), ("ru", "something")]
                                             , Mp.fromList [("en", "something2"), ("ru", "something2")] ] }
                , ChapterLeaf { chapterName = "Chapter 2"
                              , chapterDescription = Just "Sub chapter 2"
                              , paragraphs = [ Mp.fromList [("en", "something"), ("ru", "")]
                                             , Mp.fromList [("en", "something2"), ("ru", "")] ] }
                ])
                
    it "tested against 2 Chapters vs. 3 Chapter" $
        (sbJSON "" $ defaultAlloy "" "" en noMoreChapters) `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" ".sb" $
                [ ChapterLeaf { chapterName = "Chapter 1"
                              , chapterDescription = Just "Sub chapter 1"
                              , paragraphs = [ Mp.fromList [("en", "something"), ("ru", "something")]
                                             , Mp.fromList [("en", "something2"), ("ru", "something2")] ] }
                , ChapterLeaf { chapterName = "Chapter 2"
                              , chapterDescription = Just "Sub chapter 2"
                              , paragraphs = [ Mp.fromList [("en", "something"), ("ru", "something")]
                                             , Mp.fromList [("en", "something2"), ("ru", "something2")] ] }
                , ChapterLeaf { chapterName = ""
                              , chapterDescription = Just ""
                              , paragraphs = [ Mp.fromList [("en", ""), ("ru", "something")]
                                             , Mp.fromList [("en", ""), ("ru", "something2")] ] }
                ])

    it "tested against 2 Paragraphs vs. 1 Paragraph" $
        (sbJSON "" $ defaultAlloy "" "" en noLessPars) `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" ".sb" $
                [ ChapterLeaf { chapterName = "Chapter 1"
                              , chapterDescription = Just "Sub chapter 1"
                              , paragraphs = [ Mp.fromList [("en", "something"), ("ru", "something")]
                                             , Mp.fromList [("en", "something2"), ("ru", "")] ] }
                , ChapterLeaf { chapterName = "Chapter 2"
                              , chapterDescription = Just "Sub chapter 2"
                              , paragraphs = [ Mp.fromList [("en", "something"), ("ru", "something")]
                                             , Mp.fromList [("en", "something2"), ("ru", "something2")] ] }
                ])
                
    it "tested against 2 Paragraphs vs. 3 Paragraph" $
        (sbJSON "" $ defaultAlloy "" "" en noMorePars) `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" ".sb" $
                [ ChapterLeaf { chapterName = "Chapter 1"
                              , chapterDescription = Just "Sub chapter 1"
                              , paragraphs = [ Mp.fromList [("en", "something"), ("ru", "something")]
                                             , Mp.fromList [("en", "something2"), ("ru", "something2")] 
                                             , Mp.fromList [("en", ""), ("ru", "something3")] ] }
                , ChapterLeaf { chapterName = "Chapter 2"
                              , chapterDescription = Just "Sub chapter 2"
                              , paragraphs = [ Mp.fromList [("en", "something"), ("ru", "something")]
                                             , Mp.fromList [("en", "something2"), ("ru", "something2")] ] }
                ])

    let unalignedBookEn = "<h1>Chapter 1</h1><p>Mr. Sherlock Holmes</p><p>sometjkjdfgd.</p><p>\"Well, Watson, what do you make of it?\"</p><h1>Chapter 2</h1><p>some text</p>"
    let unalignedBookNo = "<p><br></p><p><br></p><p><br></p><p><br></p><h1>Kapittel 1</h1><p>Sjerlok Holms</p><p>sometjkjdfgd.</p><p>\"Brå, Vatson\"</p><p><br></p><p><br></p><p><br></p><p><br></p><h1>Kapittel 2</h1><p>noe text</p>"
    it "tested against 'unaligned' books" $
        (sbJSON "" $ defaultAlloy "" "" unalignedBookEn unalignedBookNo) `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" ".sb" $
                [ ChapterLeaf { chapterName = "Chapter 1"
                              , chapterDescription = Just ""
                              , paragraphs = [ Mp.fromList [("en", "Mr. Sherlock Holmes"), ("ru", "Sjerlok Holms")]
                                             , Mp.fromList [("en", "sometjkjdfgd."), ("ru", "sometjkjdfgd.")] 
                                             , Mp.fromList [("en", "\"Well, Watson, what do you make of it?\""), ("ru", "\"Brå, Vatson\"")] ] }
                , ChapterLeaf { chapterName = "Chapter 2"
                              , chapterDescription = Just ""
                              , paragraphs = [ Mp.fromList [("en", "some text"), ("ru", "noe text")] ] }
                ])
    
  -- adding .sb is important because otherwise SmartBook simply turns on the Google Translation instead of ours
  describe "SmartBook.sbJSON extension test" $ do

    it "tested against empty filename" $
        (sbJSON "" $ defaultAlloy "" "" "" "") `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" ".sb" $ [])

    it "tested against no extension" $
        (sbJSON "a" $ defaultAlloy "" "" "" "") `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" "a.sb" $ [])

    it "tested against non-sb extension" $
        (sbJSON "a.ext" $ defaultAlloy "" "" "" "") `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" "a.sb" $ [])

    it "tested against two extensions" $
        (sbJSON "a.ext.ext" $ defaultAlloy "" "" "" "") `shouldBe` 
            (Right . SmartBook.Alloy.defaultBook "" "" "a.ext.sb" $ [])
            
  where defaultAlloy title author en no = 
            AlloyBook { bookTitle = title
                      , bookAuthor = author
                      , bookLeft = en
                      , bookRight = no }

      
bookParserEmptyTests = do
  describe "SmartBook.AlloyParser various empty input" $ do
    let empty = ""
    it "no input" $
      (runBookParser empty) `shouldBe` 
            (Right $ [ ] )
            
    let emptyParagraph = "<p></p>"
    it emptyParagraph $
      (runBookParser . TL.pack $ emptyParagraph) `shouldBe` 
            (Right $ [ ] )
            
    let emptyParagraph2 = "<p>   <br></p>"
    it emptyParagraph2 $
      (runBookParser . TL.pack $ emptyParagraph2) `shouldBe` 
            (Right $ [ ] )
            
    let emptyParagraph3 = "<h1></h1><p>   <br></p>"
    it emptyParagraph3 $
      (runBookParser . TL.pack $ emptyParagraph3) `shouldBe` 
            (Right $ [ ] )
            
    let chapterTitleOnly = "<h1></h1>"
    it chapterTitleOnly $
      (runBookParser . TL.pack $ chapterTitleOnly) `shouldBe` 
            (Right $ [ ] )
            
    let chapterTitleOnly2 = "<h1><br></h1>"
    it chapterTitleOnly2 $
      (runBookParser . TL.pack $ chapterTitleOnly2) `shouldBe` 
            (Right $ [ ] )
            
    let chapterFullEmpty = "<h1></h1> <h2></h2>"
    it chapterFullEmpty $
      (runBookParser . TL.pack $ chapterFullEmpty) `shouldBe` 
            (Right $ [ ] )
            
    let fullEmpty = "<h1></h1> <h2></h2> <p></p>"
    it fullEmpty $
      (runBookParser . TL.pack $ fullEmpty) `shouldBe` 
            (Right $ [ ] )

    let str1 = "<h1></h1> <p><br></p> <h2></h2> <p></p>"
    it str1 $
      (runBookParser . TL.pack $ str1) `shouldBe` 
            (Right $ [ ] )
            
    let lotsOfEmpty = "\n\n\n\n<br> <br> <br> \n\n\n<br> <br><p></p>";
    it "multiple <br> with newline in-between" $
      (runBookParser . TL.pack $ lotsOfEmpty) `shouldBe` 
            (Right $ [ ] )
            
bookParserTests = do
  describe "SmartBook.AlloyParser various non-empty input" $ do
    let fullNonEmpty = "<h1></h1> <h2></h2> <p>something</p>"
    it "tested against a full non empty chapter" $
      (runBookParser fullNonEmpty) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = ["something"] } ] )
            
    let fullMultipleParagraphs = " <h1></h1> <h2></h2> <p>something</p> <p>something2</p>"
    it "tested against a full chapter with multiple paragraphs" $
      (runBookParser fullMultipleParagraphs) `shouldBe` 
            (Right $ [ Chp { chpTitle = "", chpDesc = "", chpParagraphs = ["something", "something2"] } ] )
            
    let fullMultipleChapters = "<h1>1</h1> <h2>11</h2> <p>something</p> <p>something2</p>  \
                                 \ <h1>2</h1> <h2>22</h2> <p>something</p> <p>something2</p>"
    it "tested against multiple chapters with multiple paragraphs" $
      (runBookParser fullMultipleChapters) `shouldBe` 
            (Right $ [ Chp { chpTitle = "1", chpDesc = "11", chpParagraphs = ["something", "something2"] }
                     , Chp { chpTitle = "2", chpDesc = "22", chpParagraphs = ["something", "something2"] } ] )
                     
    let divsInTheEnd = "<h1>1</h1><h2>1.1</h2><p>p</p><div></div>"
    it "must fail against DIV in the end" $
      (show . runBookParser $ divsInTheEnd) `shouldContain` 
            ("failed to parse in 'tag' at \"<div></div>\"")

    let unalignedBookNo = "<p><br></p><p><br></p><p><br></p><p><br></p><h1>Kapittel 1</h1><p>Sjerlok Holms</p><p>sometjkjdfgd.</p><p>\"Brå, Vatson\"</p><p><br></p><p><br></p><p><br></p><p><br></p><h1>Kapittel 2</h1><p>noe text</p>"
    it "book with a lot of newlines and <br>" $
      (runBookParser unalignedBookNo) `shouldBe` 
            (Right $ [ Chp { chpTitle = "Kapittel 1", chpDesc = "", chpParagraphs = ["Sjerlok Holms", "sometjkjdfgd.", "\"Brå, Vatson\""] }
                     , Chp { chpTitle = "Kapittel 2", chpDesc = "", chpParagraphs = ["noe text"] } ] )

    let str2 = "<h1></h1> <p><br>nonempty</p> <h2></h2> <p></p>"
    it str2 $
      (show $ runBookParser . TL.pack $ str2) `shouldContain` 
            ("failed to parse in 'tag' at \"<h2></h2> <p></p>\"")

    let str3 = "<h1>Chapter&nbsp;1</h1> <h2>Sub chapter&nbsp;1.1</h2> <p><br>non&nbsp;empty</p>"
    it str3 $
      (runBookParser . TL.pack $ str3) `shouldBe` 
            (Right $ [ Chp { chpTitle = "Chapter 1", chpDesc = "Sub chapter 1.1", chpParagraphs = ["non empty"] } ] )