{-# OPTIONS_GHC -Wall #-}

module Data.Vector.Utils (
    chunksOf,
    transpose
) where

import Data.Vector (Vector)
import qualified Data.Vector as V

chunksOf :: Vector a -> Int -> Vector (Vector a)
chunksOf vector i
  | V.null vector = V.empty
  | otherwise = V.take i vector `V.cons` chunksOf (V.drop i vector) i

transpose :: Vector (Vector a) -> Vector (Vector a)
transpose = V.fromList . go
  where go xs
          | V.null (V.head xs) = []
          | otherwise = V.map V.head xs : go (V.map V.tail xs)
