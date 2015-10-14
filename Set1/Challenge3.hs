module Challenge3 (
    main,
    crackSingleByteXor,
    naturalLanguageScore,
) where

import Data.Bits
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

import Hex
import Base64

hexstring :: String
hexstring = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

stringToFrequencies :: String -> Map Char Int
stringToFrequencies [] = M.empty
stringToFrequencies (s:ss) = M.alter updateFrequency s $ stringToFrequencies ss
  where updateFrequency Nothing = Just 1
        updateFrequency (Just f) = Just $ f + 1

englishFrequencies :: [(Char, Int)]
englishFrequencies = zip " etaoinshrdlcumwfgypbvkjxqz" [27,26..]

naturalLanguageScore :: String -> Int
naturalLanguageScore s = go $ M.toList $ stringToFrequencies s
  where eng = M.fromList englishFrequencies
        go [] = 0
        go (x:xs) = case M.lookup (fst x) eng of
            Just f -> (f * snd x) + go xs
            Nothing -> go xs

crackSingleByteXor :: String -> (String, Char)
crackSingleByteXor s = bestCandidate $ map decrypt possibleKeys
  where bestCandidate = last . sortBy (comparing (naturalLanguageScore . fst))
        decrypt key = (plaintext key, keyChar key)
          where plaintext = C.unpack . B.pack . zipWith xor input
                keyChar = head . C.unpack . B.pack
        possibleKeys = map (replicate (length input) . fromInteger) [0..255]
        input = B.unpack $ C.pack $ decodeHex s

main :: IO ()
main = print $ crackSingleByteXor hexstring

