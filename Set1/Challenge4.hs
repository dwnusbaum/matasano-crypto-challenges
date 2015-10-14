module Challenge4 (
    main
) where

import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)

import Challenge3 hiding (main)

findSingleByteXor :: [String] -> (String, Char)
findSingleByteXor xs = last $ sortBy (comparing (naturalLanguageScore . fst)) output
  where output = map crackSingleByteXor xs

main :: IO ()
main = do
    file <- readFile "data/4.txt"
    print $ findSingleByteXor $ lines file
