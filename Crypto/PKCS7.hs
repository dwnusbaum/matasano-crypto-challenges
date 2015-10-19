{-# OPTIONS_GHC -Wall #-}

module Crypto.PKCS7 (
    padPlaintext
) where

import Data.Word (Word8)

padPlaintext :: Int -> [Word8] -> [Word8]
padPlaintext blockSize text = text ++ pad padLength
  where padLength = blockSize - length text `mod` blockSize

pad :: Int -> [Word8]
pad n = replicate n (fromIntegral n)
