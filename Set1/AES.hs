{-# OPTIONS_GHC -Wall #-}

module AES (
    BlockCipher(..),
    Cipher,
    CipherMode,
    aes128,
    main
) where

import Data.Bits
import Data.List (foldl')
import Data.Word (Word8)
import Data.Vector (Vector, (!))

import qualified Data.Vector as V

import RijndaelField

type Cipher = Key -> [Word8] -> [Word8]
type Key = [Word8]
type CipherMode = BlockCipher -> Cipher
type RoundKey = Vector (Vector Word8)
type State = Vector (Vector Word8)

data BlockCipher = BlockCipher {
    decrypt :: Cipher,
    encrypt :: Cipher,
    blockSize :: Int
}

aes128 :: BlockCipher
aes128 = BlockCipher encryptAES decryptAES 16

-- Encrypts a single block of data using AES and the given key. The input must
-- have exactly 16 bytes.
encryptAES :: Cipher
encryptAES key = fromStateArray . cipher . toStateArray
  where cipher state = lastRound $ foldl' (\acc i -> aesRound i acc) (addRoundKey (keySchedule ! 0) state) [1..9]
        aesRound i = (keySchedule ! i `addRoundKey`) . mixColumns . shiftRows . subBytes
        lastRound = addRoundKey (keySchedule ! 10) . shiftRows . subBytes
        keySchedule = keyExpansion key

-- Decrypts a single block of data using AES and the given key. The input must
-- have exactly 16 bytes.
decryptAES :: Cipher
decryptAES key = fromStateArray . inverseCipher . toStateArray
  where inverseCipher = undefined
        keySchedule = keyExpansion key

-- Turn the 16 byte key into a vector of 11 round keys, where each round key is
-- a vector of 4 4-Byte words
keyExpansion :: Key -> Vector RoundKey
keyExpansion key = (loop 4 keyState) `chunksOfV` 4
  where loop 44 keys = keys
        loop i keys
          | i `mod` 4 == 0 = loop (i + 1) $ keys `V.snoc` (keys ! (i - 4) `fieldPolyAdd` ((subWord $ rotWord temp) `fieldPolyAdd` rCon (i `div` 4)))
          | otherwise = loop (i + 1) $ keys `V.snoc` (keys ! (i - 4) `fieldPolyAdd` temp)
          where temp = keys ! (i - 1)
        keyState = V.fromList key `chunksOfV` 4

-- Substitute every byte in vector of bytes using the sBox
subWord :: Vector Word8 -> Vector Word8
subWord = V.map (\byte -> (sBox ! upperHalf byte) ! lowerHalf byte)
  where upperHalf = fromIntegral . (`shiftR` 4)
        lowerHalf = fromIntegral . (.&. 0x0F)

-- Substitute every byte in the state using the sBox
subBytes :: State -> State
subBytes = V.map subWord

-- The sBox (Substitution box) is a 15x15 element matrix used by the subBytes
-- and subWord functions.
sBox :: Vector (Vector Word8)
sBox = V.fromList [
        V.fromList [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76],
        V.fromList [0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0],
        V.fromList [0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15],
        V.fromList [0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75],
        V.fromList [0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84],
        V.fromList [0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf],
        V.fromList [0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8],
        V.fromList [0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2],
        V.fromList [0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73],
        V.fromList [0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb],
        V.fromList [0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79],
        V.fromList [0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08],
        V.fromList [0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a],
        V.fromList [0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e],
        V.fromList [0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf],
        V.fromList [0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]
    ]

-- Rotates each row of the state to the left according to the index of the row.
-- So the 0th row rotates 0 times, the 1st row rotates 1 times, etc.
shiftRows :: State -> State
shiftRows state = V.imap (\i -> applyN i rotWord) state

-- Mutliplies the columns by the fixed, invertible polynomial in the Rijndael
-- finite field.
mixColumns :: State -> State
mixColumns = transposeV . V.map multiplyFixed . transposeV
  where multiplyFixed = fieldPolyMultiply $ V.fromList [0x02, 0x01, 0x01, 0x03]

-- Adds the round key to the state
addRoundKey :: RoundKey -> State -> State
addRoundKey roundKey = transposeV . V.zipWith fieldPolyAdd roundKey . transposeV

-- Transforms a list of bytes into the state array used by the cipher
toStateArray :: [Word8] -> State
toStateArray xs = V.fromList $ map rows [0..3]
  where rows r = V.fromList $ map (\c -> vs ! (r + 4 * c)) [0..3]
        vs = V.fromList xs

-- Transforms the state array back into a list of bytes
fromStateArray :: State -> [Word8]
fromStateArray xs = concatMap columns [0..3]
  where columns c = map (\r -> (xs ! r) ! c) [0..3]

-- Vector equivalent of chunksOf
chunksOfV :: Vector a -> Int -> Vector (Vector a)
chunksOfV vector i
  | V.null vector = V.empty
  | otherwise = V.take i vector `V.cons` chunksOfV (V.drop i vector) i

-- Vector equivalent of transpose
transposeV :: Vector (Vector a) -> Vector (Vector a)
transposeV = V.fromList . go
  where go xs
          | V.null (V.head xs) = []
          | otherwise = V.map V.head xs : go (V.map V.tail xs)

main :: IO ()
main = print $ (map showHex) $ encryptAES key state
  where key = [0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c]
        state = [0x32, 0x43 , 0xf6 , 0xa8 , 0x88 , 0x5a , 0x30 , 0x8d , 0x31 , 0x31 , 0x98 , 0xa2 , 0xe0 , 0x37 , 0x07 , 0x34]
        showHex b = (alphabet ! upperHalf b) : alphabet ! (lowerHalf b) : []
          where upperHalf = fromIntegral . (`shiftR` 4)
                lowerHalf = fromIntegral . (.&. 0x0F)
                alphabet = V.fromList $ ['0'..'9'] ++ ['a'..'f']
