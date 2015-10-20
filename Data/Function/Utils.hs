{-# OPTIONS_GHC -Wall #-}

module Data.Function.Utils (
    applyN
) where

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f $ f x
