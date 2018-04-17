{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
module Main where

import GoogleTranslate
import Lexin
 
import Control.Monad hiding (when)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.UTF8 as BU
import Blaze.ByteString.Builder (copyByteString)

-----

import qualified Text.Show.Unicode as US 
 
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app
 
app :: Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
app req respond = join $ respond <$>
    case pathInfo req of
--        ["yay"] -> yay
        ("goo":[x]) -> do
                        tx <- googleTranslateWithT . T.fromStrict $ x
                        let xxs = either (return . T.pack . US.ushow) (return . T.pack . US.ushow) tx
                        index <$> xxs
        ("lex":[x]) -> do
                        tx <- lexinTranslate . T.fromStrict $ x
                        -- take 2 translation blocks for test
                        let (ttx :: [T.Text]) = take 2 . map (T.pack . US.ushow) $ tx
                        return $ index ttx
{-        [x] -> do
                   let tx = trans x
                   index <$> tx
        x -> do
                putStrLn . show $ x
                return . index $ x
 
 
yay = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "yay" ]
-}
--index :: [T.Text] -> Network.Wai.Response
index x = responseBuilder status200 [("Content-Type", "text/html; charset=UTF-8")] $ mconcat $ map copyByteString
    [ "<p>", BU.fromString .US.ushow  $ x, "</p>" ]

