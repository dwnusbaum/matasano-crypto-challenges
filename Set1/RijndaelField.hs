module RijndaelField (
    fieldAdd,
    fieldMultiply,
    fieldPolyAdd,
    fieldPolyMultiply,
    multiplyFixed,
    multiplyFixedInverse,
    rotWord
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
  | b `mod` 0x02 == 0 = xtime a `fieldMultiply` (b `div` 2)
  | otherwise = foldl' (\acc x -> fieldMultiply a (bit x) `xor` acc) 0x0 $ bitIndices b
  where xtime a = if a `testBit` 7
            then (a `shiftL` 1) `xor` 0x1b
            else a `shiftL` 1
        bitIndices = flip filter [7,6..0] . testBit

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

-- | Multiplies a polynomial whose coefficients are elements of the Rijndael
-- finite field by a fixed element, so that the multiplication can be inverted.
multiplyFixed :: Vector Word8 -> Vector Word8
multiplyFixed = fieldPolyMultiply $ V.fromList [0x02, 0x01, 0x01, 0x03]

-- | Multiplies a polynomial whose coefficients are elements of the Rijndael
-- finite field by the inverse of a fixed element.
multiplyFixedInverse :: Vector Word8 -> Vector Word8
multiplyFixedInverse = fieldPolyMultiply $ V.fromList [0x0e, 0x09, 0x0d, 0x0b]

-- Rotates a polynomial whose coefficients are elements of the Rijndael finite
-- field to the left.
rotWord :: Vector Word8 -> Vector Word8
rotWord = fieldPolyMultiply $ V.fromList [0x00, 0x00, 0x00, 0x01]
