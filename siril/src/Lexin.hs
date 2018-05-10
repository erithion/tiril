{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
module Lexin 
where

import           Type
import           Text.XML.HXT.HTTP
import           Text.XML.HXT.Core                      hiding (app)
import qualified Text.XML.HXT.DOM.XmlNode       as XN
import qualified Text.XML.HXT.DOM.QualifiedName as XN
import qualified Data.Text.Lazy                 as T
import           Data.Char
import           Data.List
import           System.CPUTime
import           Text.Printf

data LexinWord = LexinWord
    { lexinWord :: T.Text
    , lexinLang :: T.Text
    , lexinType :: T.Text
    }
    deriving (Show, Eq)

instance Translator LexinWord where
    source = const ""
    target = T.unpack . lexinWord
    lang = T.unpack . lexinLang
    verb = T.unpack . lexinType
    iam = const "Lexin"
    
{- data-types could be 
      data-type="LEM" seems to be words - "lemma"; With LEM in the block "MOR", "DEF", "ALT" might be present
      data-type="MOR" seems to be "bøyning"
      data-type="DEF" seems to be "forklaring"
      data-type="EKS" - example - all "EKS" within one block
      data-type="IDI" - idiomatic - all "IDI" within one block
      data-type="SMS" - sammensetning - two or more words - all "SMS" within one block
-}
    
{- TODO
1. request words count first and everything on a single page up front
2. think of a browser resending the request upon long time response
3. REMOVE ALL BLOCKS WHICH DO NOT CONTAIN SEARCHED WORD
5. consider long time query processing 
6. implement other languages and directions 
7. make error handling similar to google translate -}
lexinTranslate :: T.Text -> IO [[LexinWord]]
lexinTranslate x =
    do
        let remote = "http://lexin.udir.no/?search=" ++ T.unpack x ++ "&dict=nbo-ru-maxi&ui-lang=NBO&startingfrom=&count=100"
        let doc = {-# SCC "Lexin_parser_read_http" #-} readDocument [withHTTP [], withParseHTML yes, withWarnings no, withInputEncoding utf8 ] remote
        (xxs' :: [[LexinWord]]) 
            <- {-# SCC "Lexin_parser" #-} runX $ doc >>> (deep ( hasName "table" >>> hasAttrValue "id" (=="tblHitsTable") /> hasName "tr" )) 
                           >>. map (XN.mkElement (XN.mkName "translation block") []) . groupBlocks 
                           >>> deep ( proc elem -> do 
                                        e <- (hasName "td" >>> hasAttr "data-type" >>> hasAttr "data-lang") -< elem
                                        e <- notContaining this (Text.XML.HXT.Core.getChildren >>> (hasAttrValue "class" (=="clsUtt") <+> hasAttrValue "class" (=="clsKat") ) ) -< e
                                        datatype <- getAttrValue "data-type" -< e
                                        datalang <- getAttrValue "data-lang" -< e
                                        word <- listA (multi getText) >>> arr (filter (not . isControl) . concat) -< e
                                        returnA -< LexinWord { lexinWord = (T.strip . T.pack) word
                                                             , lexinLang = (T.strip . T.pack) datalang
                                                             , lexinType = (T.strip . T.pack) datatype }
                                    ) >>. (:[])
        return xxs'
    where
        isSeparator ar =  (null $ (runLA getAttrl $ ar)) 
                       || (not . null $ (runLA (hasAttrValue "class" (isInfixOf "separator")) $ ar))
                       
        groupBlocks :: [XmlTree] -> [[XmlTree]]                                                   
        groupBlocks [] = []
        groupBlocks ls =
            let 
                (sep, tail) = span isSeparator ls
                (body, ts) = break isSeparator tail
            in (sep ++ body):groupBlocks ts