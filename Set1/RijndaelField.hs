{-# OPTIONS_GHC -Wall #-}

module RijndaelField (
    fieldAdd,
    fieldMultiply,
    fieldPolyAdd,
    fieldPolyMultiply,
    rCon,
    rotWord,
    applyN
) where

import Data.Bits
import Data.List (foldl')
import Data.Word (Word8)
import Data.Vector (Vector, (!))

import qualified Data.Vector as V

-- Add 2 bytes in the Rijndael finite field
fieldAdd :: Word8 -> Word8 -> Word8
fieldAdd = xor

-- Multiply 2 bytes in the Rijndael finite field
fieldMultiply :: Word8 -> Word8 -> Word8
fieldMultiply a b
  | b == 0x01 = a
  | popCount b == 1 = applyN (log2 $ fromIntegral b) xtime a
  | otherwise = foldl' (\acc x -> fieldMultiply a (bit x) `xor` acc) 0x0 $ bitIndices b
  where bitIndices = flip filter [7,6..0] . testBit

xtime :: Word8 -> Word8
xtime x = if x `testBit` 7
            then (x `shiftL` 1) `xor` 0x1b
            else x `shiftL` 1

log2 :: Int -> Int
log2 n
  | n < 1 = error "log2: argument must be positive"
  | otherwise = go 0 1
  where
    go pow prod
      | prod < n = go (pow + 1) (2 * prod)
      | otherwise = pow

-- Add 2 polynomials whose coefficients are elements of the Rijndael finite
-- field.
fieldPolyAdd :: Vector Word8 -> Vector Word8 -> Vector Word8
fieldPolyAdd = V.zipWith fieldAdd

-- Mulitply 2 polynomials whose coefficients are elements of the Rijndael
-- finite field.
fieldPolyMultiply :: Vector Word8 -> Vector Word8 -> Vector Word8
fieldPolyMultiply a b = V.fromList [
        ((a ! 0) `fieldMultiply` (b ! 0)) `xor` ((a ! 3) `fieldMultiply` (b ! 1)) `xor` ((a ! 2) `fieldMultiply` (b ! 2)) `xor` ((a ! 1) `fieldMultiply` (b ! 3)),
        ((a ! 1) `fieldMultiply` (b ! 0)) `xor` ((a ! 0) `fieldMultiply` (b ! 1)) `xor` ((a ! 3) `fieldMultiply` (b ! 2)) `xor` ((a ! 2) `fieldMultiply` (b ! 3)),
        ((a ! 2) `fieldMultiply` (b ! 0)) `xor` ((a ! 1) `fieldMultiply` (b ! 1)) `xor` ((a ! 0) `fieldMultiply` (b ! 2)) `xor` ((a ! 3) `fieldMultiply` (b ! 3)),
        ((a ! 3) `fieldMultiply` (b ! 0)) `xor` ((a ! 2) `fieldMultiply` (b ! 1)) `xor` ((a ! 1) `fieldMultiply` (b ! 2)) `xor` ((a ! 0) `fieldMultiply` (b ! 3))
    ]

-- Rotates a polynomial whose coefficients are elements of the Rijndael finite
-- field to the left.
rotWord :: Vector Word8 -> Vector Word8
rotWord = fieldPolyMultiply $ V.fromList [0x00, 0x00, 0x00, 0x01]

rCon :: Int -> Vector Word8
rCon i = V.fromList [applyN (i - 1) (fieldMultiply 0x02) 0x01, 0x0, 0x0, 0x0]

applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)
