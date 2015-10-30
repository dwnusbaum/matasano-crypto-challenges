{-# OPTIONS_GHC -Wall #-}

module Crypto.AES (
    BlockCipher(..),
    InitializationVector,
    decrypt_AES128_ECB,
    encrypt_AES128_ECB,
    decrypt_AES128_CBC,
    encrypt_AES128_CBC
) where

import Data.Bits
import Data.Word (Word8)

import Crypto.AES.Cipher
import Data.List.Utils

type Action = BlockCipher -> Cipher
type InitializationVector = [Word8]

ecb :: BlockCipher -> Action -> Cipher
ecb cipher action key input = concatMap (action cipher key) blocks
  where blocks = padNulls input `chunksOf` blockSize cipher
        padNulls text = case length text `mod` blockSize cipher of
            0 -> text
            n -> text ++ replicate (blockSize cipher - n) 0

decrypt_AES128_ECB :: Cipher
decrypt_AES128_ECB = ecb aes128 decrypt

encrypt_AES128_ECB :: Cipher
encrypt_AES128_ECB = ecb aes128 encrypt

decrypt_AES128_CBC :: InitializationVector -> Cipher
decrypt_AES128_CBC iv key input = verifyBlockSize aes128 input $ go iv blocks
  where go _ [] = []
        go lastCiphertext (c:cs) = plaintext ++ go c cs
          where plaintext = zipWith xor lastCiphertext $ decrypt aes128 key c
        blocks = input `chunksOf` blockSize aes128

encrypt_AES128_CBC :: InitializationVector -> Cipher
encrypt_AES128_CBC iv key input = verifyBlockSize aes128 input $ go iv blocks
  where go _ [] = []
        go lastCiphertext (p:ps) = ciphertext ++ go ciphertext ps
          where ciphertext = encrypt aes128 key $ zipWith xor p lastCiphertext
        blocks = input `chunksOf` blockSize aes128

verifyBlockSize :: BlockCipher -> [Word8] -> [Word8] -> [Word8]
verifyBlockSize cipher input x = if length input `mod` blockSize cipher == 0
    then x
    else error "AES.hs: Input was not a multiple of the block size. You need to pad the input before encrypting it."
