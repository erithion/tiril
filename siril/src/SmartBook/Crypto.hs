{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SmartBook.Crypto
( encrypt
, decrypt )
where

import Crypto.Cipher.AES
import Data.Tuple.Extra                                 ( (&&&) )
import Control.Applicative                              ( (<$>), (<*>), (<|>) )
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Base64     as B64
import qualified Data.Text.Lazy.IO          as TexL
import qualified Data.Text.Lazy             as TexL
import qualified Data.Text.Lazy.Encoding    as TexL

-- They say that Java's Cipher.getInstance("AES") by default uses Cipher.getInstance("AES/ECB/PKCS5Padding");
aes :: AES
aes = initAES ("sbsecretsbsecret" :: BS.ByteString)

-- TODO: Replace with lazy encryption and remove BSL.toStrict . TexL.encodeUtf8
decrypt :: TexL.Text -> Either String BS.ByteString
decrypt = (<$>) (removePadding . decryptECB aes) . B64.decode . BSL.toStrict . TexL.encodeUtf8 
    where removePadding :: BS.ByteString -> BS.ByteString
          removePadding bs
            | BS.null bs        = bs
            | BS.last bs <= 15  = let count = fromIntegral . BS.last $ bs
                                      suffix = BS.replicate count (toEnum count) 
                                  in case BS.stripSuffix suffix bs of
                                            -- Padding found
                                            Just xs -> xs
                                            -- There were no repeating bytes hence no padding
                                            Nothing -> bs
            | otherwise         = bs

-- TODO: Replace with lazy encryption and remove BSL.toStrict . TexL.encodeUtf8
encrypt :: TexL.Text -> BS.ByteString          
encrypt input = B64.encode . encryptECB aes $
                    (uncurry BS.append . (id &&& paddingArray . BS.length) . BSL.toStrict . TexL.encodeUtf8 $ input)
    where 
        -- for AES ECB the data must be of the multiple 16 size
        paddingArray :: Int -> BS.ByteString
        paddingArray size = 
            let count = case mod size 16 of 
                                0 -> 0
                                x -> fromIntegral $ 16 - x
            --  Padding: The value of each added byte is the number of bytes that are added, for example 03 03 03 or 04 04 04 04
            in BS.replicate count (toEnum count)