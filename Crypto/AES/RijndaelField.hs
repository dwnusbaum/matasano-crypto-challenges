{-# OPTIONS_GHC -Wall #-}

module Crypto.AES.RijndaelField (
    fieldAdd,
    fieldMultiply,
    fieldPolyAdd,
    fieldPolyMultiply,
    rCon,
    rotWord
) where

import Data.Bits
import Data.List (foldl')
import Data.Word (Word8)
import Data.Vector (Vector, (!))

import qualified Data.Vector as V

import Data.Function.Utils

-- Add 2 bytes in the Rijndael finite field
fieldAdd :: Word8 -> Word8 -> Word8
fieldAdd = xor

-- Multiply 2 bytes in the Rijndael finite field
fieldMultiply :: Word8 -> Word8 -> Word8
fieldMultiply a b
  | b == 0x01 = a
  | popCount b == 1 = applyN (log2 b) xtime a
  | otherwise = foldl' sumMultiplyBits 0x0 [7,6..0]
  where sumMultiplyBits acc i
          | b `testBit` i = fieldMultiply a (bit i) `fieldAdd` acc
          | otherwise = acc

xtime :: Word8 -> Word8
xtime x
  | x `testBit` 7 = (x `shiftL` 1) `xor` 0x1b
  | otherwise = x `shiftL` 1

-- Only works for powers of 2!!
log2 :: Word8 -> Int
log2 0x80 = 8
log2 0x40 = 7
log2 0x20 = 6
log2 0x10 = 5
log2 0x08 = 4
log2 0x06 = 3
log2 0x04 = 2
log2 0x02 = 1
log2 _ = error "RijndaelField.hs: Input was not a power of 2"

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

-- Returns the round constant word array
rCon :: Int -> Vector Word8
rCon i = V.fromList [applyN (i - 1) (fieldMultiply 0x02) 0x01, 0x0, 0x0, 0x0]
