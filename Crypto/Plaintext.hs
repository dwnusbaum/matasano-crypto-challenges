{-# OPTIONS_GHC -Wall #-}

module Crypto.Plaintext (
    naturalLanguageScore
) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M

stringToFrequencies :: String -> Map Char Double
stringToFrequencies = go
  where go [] = M.empty
        go (s:ss) = M.alter updateFrequency (toLower s) $ stringToFrequencies ss
          where updateFrequency Nothing = Just 1
                updateFrequency (Just f) = Just $ f + 1

englishFrequencies :: Map Char Double
englishFrequencies = M.fromList $ zip "abcdefghijklmnopqrstuvwxyz " frequencies
  where frequencies = [0.0651738,0.0124248,0.0217339,0.0349835,0.1041442,0.0197881,0.0158610,0.0492888,0.0558094,0.0009033,0.0050529,0.0331490,0.0202124,0.0564513,0.0596302,0.0137645,0.0008606,0.0497563,0.0515760,0.0729357,0.0225134,0.0082903,0.0171272,0.0013692,0.0145984,0.0007836,0.1918182]

naturalLanguageScore :: String -> Double
naturalLanguageScore s = go . M.toList . stringToFrequencies $ s
  where sLen = fromIntegral (length s)
        go [] = 0
        go ((x, xFreq):xs) = case M.lookup (toLower x) englishFrequencies of
            Just engFreq -> chiSquaredDistance xFreq (engFreq * sLen) + go xs
            Nothing -> chiSquaredDistance xFreq 0 + go xs

chiSquaredDistance :: Double -> Double -> Double
chiSquaredDistance obsFreq expFreq = ((obsFreq - expFreq) ^ (2 :: Int)) / (obsFreq + expFreq)
