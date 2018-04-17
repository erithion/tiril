{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
module Lexin 
--    ( someFunc
--    ) 
where

import Type
 
import Text.XML.HXT.HTTP
import Text.XML.HXT.Core hiding (app)
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.DOM.QualifiedName as XN
import qualified Data.Text.Lazy as T

import Data.Char

--import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
 
data LexinWord = LexinWord
    { lexinWord :: T.Text
    , lexinLang :: T.Text
    , lexinType :: T.Text
    }
    deriving (Show)
 
-- First collect into blocks everything in between empty <TR>

-- TODO : 
-- request words count first and request everything on a single page
-- REMOVE ALL BLOCKS WHICH DO NOT CONTAIN SEARCHED WORD
-- Within those blocks collect all <TD data-type=...>. They all must coincide, for example "LEM". data-lang will contain the language: N - norsk, B-english???, RU-russian
--      data-type="LEM" seems to be words - "lemma"; With LEM in the block "MOR", "DEF", "ALT" can be present
--      data-type="MOR" seems to be "bøyning"
--      data-type="DEF" seems to be "forklaring"
--      data-type="EKS" - example - all "EKS" within one block
--      data-type="IDI" - idiomatic - all "IDI" within one block
--      data-type="SMS" - sammensetning - two or more words - all "SMS" within one block
-- Within those collected TD's filter out ones with <SPAN class="clsKat"> as children - those are entries with "verb", "глагол" etc

-- consider the possibility of lazy match groups implementation 
-- consider long query processing

   
lexinTranslate :: T.Text -> IO [[LexinWord]]
lexinTranslate x =
    do

        let remote = "http://lexin.udir.no/?search=" ++ T.unpack x ++ "&dict=nbo-ru-maxi&ui-lang=NBO&startingfrom=&count=100"
--        let remote = ".//hallo2.html"
        let doc = readDocument [withHTTP [], withParseHTML yes, withWarnings no, withInputEncoding utf8] remote

        putStrLn "In lex"
        
--        xxs' <-  runXIOState (initialState []) $ doc >>> (deep ( hasName "table" >>> hasAttrValue "id" (=="tblHitsTable") /> hasName "tr" )) 
        (xxs' :: [[LexinWord]]) <-  runX $ doc >>> (deep ( hasName "table" >>> hasAttrValue "id" (=="tblHitsTable") /> hasName "tr" )) 
                                        >>. map (XN.mkElement (XN.mkName "translation block") []) . groups 
                                        >>> deep ( proc elem -> do 
                                                      e <- (hasName "td" >>> hasAttr "data-type" >>> hasAttr "data-lang") -< elem
                                                      e <- notContaining this (Text.XML.HXT.Core.getChildren >>> 
                                                                              (hasAttrValue "class" (=="clsUtt") <+> hasAttrValue "class" (=="clsKat") ) ) -< e
                                                      datatype <- getAttrValue "data-type" -< e
                                                      datalang <- getAttrValue "data-lang" -< e
                                                      word <- listA (multi getText) >>> arr (filter (not . isControl) . concat) -< e
                                                      returnA -< LexinWord { lexinWord = (T.strip . T.pack) word
                                                                           , lexinLang = (T.strip . T.pack) datalang
                                                                           , lexinType = (T.strip . T.pack) datatype }
                                                 ) >>. (:[])
                  
                                    
        return xxs'
    
-- useful modules
--     Text.XML.HXT.DOM.XmlNode предикаты, геттеры
--     Control.Arrow.ArrowTree  работа с деревом
--     Control.Arrow.ArrowList  стрелки, списки стрелок
--     Control.Arrow.ArrowIf    комбинаторы
--     Text.XML.HXT.Arrow.XmlArrow  стрелки-геттеры и стрелки-предикаты
-- help https://wiki.haskell.org/Arrow_tutorial
-- https://wiki.haskell.org/HXT/Practical/Google1
-- https://wiki.haskell.org/HXT#The_basic_concepts
-- разобраться как читать стрелки по типам и что они делают
    
    where
{-        groups s                 =  cons (case break (has "class" "clsE separatorTop separatorThick") s of
                                            (l, s') -> (l, case s' of
                                                            []      -> []
                                                            _:s''   -> groups s''))
        cons ~(h, t)        =  h : t    
                                                            -}
        groups :: [XmlTree] -> [[XmlTree]]                                                   
        groups ls = reverse $ reverse <$> (groups' . reverse $ ls) -- separator always goes first, so we reverse the list for groups' to work properly
        groups' []                =  []
        groups' ls = let 
                           e@(f, s) = break isSeparator ls
                           (f', s') = case s of 
                                        []        -> e
                                        (n:ls)    -> (f ++ [n], ls)
                     in f':(groups' s')
        isSeparator ar =  (not . null $ (runLA (hasAttrValue "class" (=="sep")) $ ar))
                       || (null $ (runLA getAttrl $ ar))
                                                 
{------------- FOR LONG QUERY TO IMPLEMENT PARSING WITH GUIDS IN SOME POOL WHEN BROWSER REPEATS ITS QUERY        
lexinTranslate :: T.Text -> IO [T.Text]
lexinTranslate x =
    do

        let remote = "http://lexin.udir.no/?search=lete&dict=nbo-ru-maxi&ui-lang=NBO&startingfrom=&count=100"
        let doc = readDocument [withHTTP [], withParseHTML yes, withWarnings no, withInputEncoding utf8] remote

--        xxs <- runX $ doc >>> deep (isElem >>> hasName "strong" 
--                      >>> withTraceLevel 4 (traceDoc "resulting document")
--                      ) //> getText
        putStrLn "In lex"
        xxs <- runX $ 
                doc >>> deep 
                        ( isElem >>> hasName "tr" >>>
                          ( 
--                           isA XN.isAttr
--                           <+>
                           hasAttr "class" --(=="clsLabel separatorTop separatorThick")                           
                           )
                        ) //> getChildren
               
        let xxxs = join $ mapM (return . T.pack . US.ushow ) xxs
        return xxxs
        
-}