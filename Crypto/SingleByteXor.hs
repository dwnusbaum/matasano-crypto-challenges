{-# OPTIONS_GHC -Wall #-}

module Crypto.SingleByteXor (
    crackSingleByteXor,
    detectSingleByteXor
) where

import Data.Bits
import Data.List (minimumBy)
import Data.Ord (comparing)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Crypto.Plaintext

crackSingleByteXor :: String -> (String, Char)
crackSingleByteXor s = bestCandidate $ map decrypt possibleKeys
  where bestCandidate = minimumBy (comparing (naturalLanguageScore . fst))
        decrypt key = (plaintext key, keyChar key)
          where plaintext = C.unpack . B.pack . zipWith xor input
                keyChar = head . C.unpack . B.pack
        possibleKeys = map (replicate (length input) . fromInteger) [0..255]
        input = B.unpack $ C.pack s

detectSingleByteXor :: [String] -> (String, Char)
detectSingleByteXor xs = minimumBy (comparing (naturalLanguageScore . fst)) output
  where output = map crackSingleByteXor xs
