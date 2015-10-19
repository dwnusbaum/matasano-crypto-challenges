{-# OPTIONS_GHC -Wall #-}

module Crypto.Vignerre (
    crackRepeatingKeyXor,
    repeatingKeyXor
) where

import Data.Bits
import Data.List (minimumBy, sortBy, transpose)
import Data.Ord (comparing)
import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Crypto.Plaintext
import Crypto.SingleByteXor
import Data.List.Utils

crackRepeatingKeyXor :: String -> (String, String)
crackRepeatingKeyXor ciphertext = best $ map (\k -> (repeatingKeyXor ciphertext k, k)) possibleKeys
    where best = minimumBy (comparing (naturalLanguageScore . fst))
          keyLengths = take 4 $ possibleKeyLengths ciphertext
          possibleKeys = map (crackGivenLengthRepeatingKeyXor ciphertext) keyLengths

repeatingKeyXor :: String -> String -> String
repeatingKeyXor string key = C.unpack $ B.pack $ process stringBytes repeatedKey
    where process [] _ = []
          process _ [] = error "Key should not have been shorter than input."
          process (s:ss) (k:ks) = s `xor` k : process ss ks
          repeatedKey = concat $ replicate (length stringBytes `div` length keyBytes + 1) keyBytes
          stringBytes = B.unpack $ C.pack string
          keyBytes = B.unpack $ C.pack key

hammingDistance :: [Word8] -> [Word8] -> Int
hammingDistance a b = sum $ map popCount $ zipWith xor a b

normalizedHammingDistance :: [Word8] -> [Word8] -> Double
normalizedHammingDistance a b = fromIntegral (hammingDistance a b) / fromIntegral (min (length a) (length b))

possibleKeyLengths :: String -> [Int]
possibleKeyLengths ciphertext = best $ map (\i -> (i, averageDistance i)) [2..40]
  where best = map fst . sortBy (comparing snd)
        averageDistance i = (distance12 + distance23 + distance13) / 3.0
          where distance12 = normalizedHammingDistance firstBlock secondBlock
                distance23 = normalizedHammingDistance secondBlock thirdBlock
                distance13 = normalizedHammingDistance firstBlock thirdBlock
                firstBlock = take i ciphertextBytes
                secondBlock = take i $ drop i ciphertextBytes
                thirdBlock = take i $ drop (2 * i) ciphertextBytes
        ciphertextBytes = B.unpack $ C.pack ciphertext

crackGivenLengthRepeatingKeyXor :: String -> Int -> String
crackGivenLengthRepeatingKeyXor ciphertext keyLength = map (snd . crackSingleByteXor) sameByteBlocks
    where ciphertextBytes = B.unpack $ C.pack ciphertext
          blocks = ciphertextBytes `chunksOf` keyLength
          sameByteBlocks = map (C.unpack . B.pack) $ transpose blocks
