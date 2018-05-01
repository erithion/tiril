module Main where

import qualified Data.Text.Lazy                 as T
import           Control.Monad                                                  hiding (when)
import qualified Control.Monad                  as M        (when)
import           Control.Applicative
import           Data.Tuple.Extra                           ((***), (&&&))
import           Data.Either
import           Control.Concurrent.Async                                       hiding (link)
import           Criterion.Main
import           GHC.Generics (Generic)
import           Control.DeepSeq

import           Type
import           GoogleTranslate
import           Lexin

instance NFData LexinWord where
    rnf a = seq a ()

instance NFData Tir where
    rnf a = seq a ()
    
main = defaultMain [
  bgroup "Translation" 
    [ bench "Sequential on \"hello\"" $ nfIO $ sequentialTranslate "hello"
    , bench "Async applicative on \"hello\"" $ nfIO $ asyncTranslateApplicative "hello"
    , bench "Async on \"hello\"" $ nfIO $ asyncTranslate "hello"
    ]
  ]

sequentialTranslate :: String -> IO ([[LexinWord]], [Tir])
sequentialTranslate = uncurry (liftA2 (,)) . (lexinTranslate &&& googleTranslate) . T.pack 
    where googleTranslate :: T.Text -> IO [Tir]
          googleTranslate word = do
            res <- googleTranslateWithT word
            M.when (isLeft res) $ do
                let (Left x) = res
                putStrLn . T.unpack $ x
            return $ either (const []) id res

asyncTranslateApplicative :: String -> IO ([[LexinWord]], [Tir])
asyncTranslateApplicative = runConcurrently . uncurry (liftA2 (,)) . (Concurrently . lexinTranslate &&& Concurrently . googleTranslate) . T.pack 
    where googleTranslate :: T.Text -> IO [Tir]
          googleTranslate word = do
            res <- googleTranslateWithT word
            M.when (isLeft res) $ do
                let (Left x) = res
                putStrLn . T.unpack $ x
            return $ either (const []) id res
            
asyncTranslate :: String -> IO ([[LexinWord]], [Tir])
asyncTranslate word = concurrently (lexinTranslate . T.pack $ word) (googleTranslate . T.pack $ word)
    where googleTranslate :: T.Text -> IO [Tir]
          googleTranslate word = do
            res <- googleTranslateWithT word
            M.when (isLeft res) $ do
                let (Left x) = res
                putStrLn . T.unpack $ x
            return $ either (const []) id res