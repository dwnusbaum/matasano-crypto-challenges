{-# OPTIONS_GHC -Wall #-}

module Challenge6 (
    main,
    hammingDistance,
    crackRepeatingKeyXor,
    chunksOf
) where

import Data.Bits
import Data.List (minimumBy, sortBy, transpose)
import Data.Ord (comparing)
import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Base64
import Challenge3 hiding (main)
import Challenge5 hiding (main)

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

chunksOf :: [a] -> Int -> [[a]]
chunksOf [] _ = [[]]
chunksOf list i = take i list : chunksOf (drop i list) i

crackRepeatingKeyXor :: String -> (String, String)
crackRepeatingKeyXor ciphertext = best $ map (\k -> (repeatingKeyXor ciphertext k, k)) possibleKeys
    where best = minimumBy (comparing (naturalLanguageScore . fst))
          keyLengths = take 4 $ possibleKeyLengths ciphertext
          possibleKeys = map (crackGivenLengthRepeatingKeyXor ciphertext) keyLengths

main :: IO ()
main = do
    file <- readFile "data/6.txt"
    let decodedFile = decodeBase64 $ filter (/= '\n') file
    print $ crackRepeatingKeyXor decodedFile
