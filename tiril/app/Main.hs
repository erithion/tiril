{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
module Main where

import qualified Data.Text.Lazy                 as T
import qualified Data.ByteString.UTF8           as BU
import qualified Text.Show.Unicode              as US 
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types                         (status200)
import           Blaze.ByteString.Builder                   (copyByteString)
import           Control.Monad                                                  hiding (when)

import           GoogleTranslate
import           Lexin
import           Session
 
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app
 
app :: Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
app req respond = join $ respond <$>
    case pathInfo req of
        ("goo":[x]) -> do
                            tx <- googleTranslateWithT . T.fromStrict $ x
                            -- TODO: Think of a proper string type to avoid those packs and repacks
                            let xxs = either (return . T.pack . US.ushow) (return . T.pack . US.ushow) tx
                            index <$> xxs
        ("lex":[x]) -> do
                            tx <- lexinTranslate . T.fromStrict $ x
                            -- TODO: Currently you take merely 2 sections. Think of a proper extension to give out all available data
                            let (ttx :: [T.Text]) = take 2 . map (T.pack . US.ushow) $ tx
                            return $ index ttx
        ("add":[x]) -> do
                            addWord . T.fromStrict $ x
                            return $ index "done"
 
 
index :: Show a => a -> Network.Wai.Response
index x = 
    responseBuilder status200 [("Content-Type", "text/html; charset=UTF-8")] $ 
        mconcat $ map copyByteString [ "<p>", BU.fromString .US.ushow  $ x, "</p>" ]

