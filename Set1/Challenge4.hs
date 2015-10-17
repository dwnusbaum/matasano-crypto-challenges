{-# OPTIONS_GHC -Wall #-}

module Challenge4 (
    main
) where

import Data.List (minimumBy)
import Data.Ord (comparing)

import Challenge3 hiding (main)

import Hex

detectSingleByteXor :: [String] -> (String, Char)
detectSingleByteXor xs = minimumBy (comparing (naturalLanguageScore . fst)) output
  where output = map (crackSingleByteXor . decodeHex) xs

main :: IO ()
main = do
    file <- readFile "data/4.txt"
    print $ detectSingleByteXor $ lines file
