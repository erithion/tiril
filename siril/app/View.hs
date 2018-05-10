{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View where

import qualified Data.Text.Lazy                 as T
import qualified Data.Text                      as TS
import           Control.Monad                                                  hiding (when)
import qualified Control.Monad                  as M        (when)
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import           Control.Concurrent                          (forkIO)
import           Data.Tuple.Extra                            ((***), (&&&))
import           Data.Either
import           Control.Concurrent.Async                                       hiding (link)
import           Data.Hashable                               (hash)

import           Type
import           GoogleTranslate
import           Lexin
import           Session
import           Db
import           Windows

jsStartWaiting :: UI ()
jsStartWaiting = runFunction $ ffi "startWaiting()"
jsEndWaiting :: UI ()
jsEndWaiting = runFunction $ ffi "endWaiting()"
jsInitSimpleScrollbar :: String -> UI ()
jsInitSimpleScrollbar = runFunction . ffi "var ps = new PerfectScrollbar(%1)"
jsInitCollapsible :: String -> UI ()
jsInitCollapsible = runFunction . ffi "initCollapsible(%1)"
jsToggleCard :: Element -> UI Int
jsToggleCard = callFunction . ffi "uiToggleCard(%1)"
jsUpdateSortable :: String -> UI ()
jsUpdateSortable = runFunction . ffi "updateSortable(%1)"
jsInitApi = do
  add <- ffiExport jsAddTranslation
  delete <- ffiExport jsDeleteTranslation
  runFunction $ ffi "initApi(%1, %2)" add delete

viewSession = const $ do
  sessionWords <- liftIO $ getWords
  clearMainWindow
  cards <- sequence (makeCard <$> sessionWords)
  getMainWindow #+ [ UI.link 
                      # set UI.rel "stylesheet"
                      # set UI.href "/static/please-wait.css"
                   , UI.link 
                      # set UI.rel "stylesheet"
                      # set UI.href "/static/view.css"
                   , UI.div #. "tiril-left-pane relative" #+ 
                      [ UI.div 
                          # set UI.id_ "collapsible" 
                          #+ join cards
                      ]
                   , mkElement "script" # set UI.src "/static/jquery.collapsible.js"
                   , mkElement "script" # set UI.src "/static/please-wait.min.js" 
                   , mkElement "script" # set UI.src "/static/view.js" 
                   ]
  -- TODO: add sortable to the left-pane
  jsInitSimpleScrollbar ".tiril-left-pane"
  jsInitCollapsible "collapsible"
  
makeCard :: NewWord w => w -> UI [UI Element]
makeCard word = do
  tr <- liftIO $ getTranslations . TS.pack . newWord $ word
  card <- mkElement "h" 
              #. "tiril-word hvr-underline-from-left d-flex flex-row-reverse justify-content-end"
              #+ [ mkElement "word" #. "m-1" # set text (newWord word)
                 , UI.span 
                      #. "badge align-self-start" 
                      # set (attr "style") "font-size:50%;" # set text (newLang word) 
                 ]
  content <- UI.div #+
              [ UI.ul #. "list" #+ (createWord 1 {- always 1 for now -}<$> tr) ]
          
  on UI.click card $ const $ do
      deleteWindows "tiril-right-pane"
      isSelectedNow <- jsToggleCard card
      if isSelectedNow /= 0 then do
          jsStartWaiting
          tr <- liftIO $ do
              let googleTranslate word = do
                      res <- googleTranslateWithT (T.pack . newLang $ word) (T.pack "ru") (T.pack . newWord $ word)
                      -- TODO: Think of better ways to output the error
                      M.when (isLeft res) $ do
                          -- We are not afraid of matching Left only,
                          -- since we had the guard "when" before coming here
                          let (Left x) = res
                          -- and we just reporting the error on the screen
                          putStrLn . T.unpack $ x
                      (return . either (const []) id $ res) 
                          :: IO [Tir]

              -- Run "stack bench" to see the results of async and sequential translation benchmarking
              (uncurry concurrently . (googleTranslate &&& lexinTranslate . T.pack . newWord) $ word) 
                  :: IO ([Tir], [[LexinWord]])

          jsEndWaiting
          -- We need a unique index for each block, 
          -- so the idea is to create a list of curry'ed functions first 
          -- so that we could zip them with a list of indices later. So neat! I love Haskell!
          let (fs :: [(Int->[UI Element])]) = uncurry (:) . (createWordPack *** (<$>) createWordPack) $ tr
          let (ts :: [UI Element]) = concat $ zipWith ($) fs [1..]
          void $ getMainWindow #+ [ UI.div #. "tiril-right-pane relative" 
                                           #+ ts ]
                                           
          jsInitApi
          jsUpdateSortable (newWord word)
          jsInitSimpleScrollbar ".tiril-right-pane"
      else return ()
  return [ element card, element content ]

createWordPack :: Translator a => [a] -> Int-> [UI Element]
createWordPack ws idx = 
  [ mkElement "header" 
      #. "title"
  , UI.ul 
      #. "tiril-detail-container" 
      # set UI.id_ ("bar_" ++ show idx)
      #+ (createWord idx <$> ws) ]

createWord :: Translator a => Int -> a -> UI Element
createWord idx w = 
  UI.li 
      #. "tiril-translation-word"
      # set (attr "hash") (show . hash . target $ w)
      #+ [ mkElement "word" 
              # set text (target w) 
         , mkElement "caps" 
              #+  [ mkElement "lem" #. "mr-1 mb-1 badge badge-dark" # set (attr "style") "font-size:50%;" # set text (verb w)
                  , mkElement "lang" #. "mr-1 mb-1 badge badge-warning" # set (attr "style") "font-size:50%;" # set text (lang w)
                  , mkElement "dict" #. "mr-1 mb-1 badge badge-info" # set (attr "style") "font-size:50%;" # set text (iam w) ] 
         , mkElement "i" 
              #. "js-remove" 
              # set (attr "data-parent-id") ("bar_" ++ show idx) 
              # set text "✖" ]