{-# OPTIONS_GHC -Wall #-}

module Data.Vector.Utils (
    chunksOf,
    transpose
) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

chunksOf :: Vector a -> Int -> Vector (Vector a)
chunksOf vector i
  | V.null vector = V.empty
  | otherwise = V.take i vector `V.cons` chunksOf (V.drop i vector) i

-- Assumes all rows have the same number of columns
transpose :: Vector (Vector a) -> Vector (Vector a)
transpose xs = V.map (\c -> V.map (\r -> (xs ! r) ! c) rowIndices) columnIndices
  where columnIndices = V.generate (V.length $ xs ! 0) id
        rowIndices = V.generate (V.length xs) id
