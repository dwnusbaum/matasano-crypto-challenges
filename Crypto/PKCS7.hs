{-# OPTIONS_GHC -Wall #-}

module Crypto.PKCS7 (
    padPlaintext
) where

import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

padPlaintext :: Int -> String -> String
padPlaintext blockSize text = C.unpack . B.pack . (++ pad padLength) . B.unpack . C.pack $ text
  where padLength = blockSize - length text `mod` blockSize

pad :: Int -> [Word8]
pad n = replicate n (fromIntegral n)
