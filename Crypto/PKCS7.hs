{-# OPTIONS_GHC -Wall #-}

module Crypto.PKCS7 (
    padPKCS7,
    validatePKCS7
) where

import Data.Word (Word8)

import qualified Data.Vector as V

padPKCS7 :: Int -> [Word8] -> [Word8]
padPKCS7 blockSize text = text ++ padding
  where padLength = blockSize - (length text `mod` blockSize)
        padding = replicate padLength (fromIntegral padLength)

validatePKCS7 :: [Word8] -> Either () [Word8]
validatePKCS7 input
  | V.all (== paddingValue) padding = Right $ V.toList noPadding
  | otherwise = Left ()
  where inputV = V.fromList input
        paddingValue = V.last inputV
        (noPadding, padding) = V.splitAt (V.length inputV - fromIntegral paddingValue) inputV
