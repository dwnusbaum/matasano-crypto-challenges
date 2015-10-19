{-# OPTIONS_GHC -Wall #-}

module Data.Function.Utils (
    applyN
) where

applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)
