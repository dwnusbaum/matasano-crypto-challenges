{-# OPTIONS_GHC -Wall #-}

module Data.List.Utils (
    chunksOf
) where

chunksOf :: [a] -> Int -> [[a]]
chunksOf [] _ = []
chunksOf list i = take i list : chunksOf (drop i list) i
