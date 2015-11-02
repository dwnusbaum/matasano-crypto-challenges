{-# OPTIONS_GHC -Wall #-}

module Data.List.Utils (
    chunksOf,
    splitOn
) where

chunksOf :: [a] -> Int -> [[a]]
chunksOf [] _ = []
chunksOf list i = take i list : chunksOf (drop i list) i

splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn list split = go list []
  where go [] acc = [reverse acc]
        go (x:xs) acc
          | x == split = reverse acc : go xs []
          | otherwise = go xs (x : acc)
