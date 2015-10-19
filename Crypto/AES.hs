{-# OPTIONS_GHC -Wall #-}

module Crypto.AES (
    BlockCipher(..),
    InitializationVector,
    decrypt_AES128_ECB,
    encrypt_AES128_ECB,
    decrypt_AES128_CBC,
    encrypt_AES128_CBC,
    detectECBModeEncryption
) where

import Data.Bits
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)

import qualified Data.Set as S

import Crypto.AES.Cipher
import Data.List.Utils

type Action = BlockCipher -> Cipher
type Ciphertext = [String]
type InitializationVector = [Word8]

ecb :: BlockCipher -> Action -> Cipher
ecb cipher action key input = concatMap (action cipher key) blocks
  where blocks = input `chunksOf` blockSize cipher

decrypt_AES128_ECB :: Cipher
decrypt_AES128_ECB = ecb aes128 decrypt

encrypt_AES128_ECB :: Cipher
encrypt_AES128_ECB = ecb aes128 encrypt

decrypt_AES128_CBC :: InitializationVector -> Cipher
decrypt_AES128_CBC iv key input = go iv blocks
  where go _ [] = []
        go lastCiphertext (c:cs) = plaintext ++ go c cs
          where plaintext = zipWith xor lastCiphertext $ decrypt aes128 key c
        blocks = input `chunksOf` blockSize aes128

encrypt_AES128_CBC :: InitializationVector -> Cipher
encrypt_AES128_CBC iv key input = go iv blocks
  where go _ [] = []
        go lastCiphertext (p:ps) = ciphertext ++ go ciphertext ps
          where ciphertext = (decrypt aes128 key) $ zipWith xor p lastCiphertext
        blocks = input `chunksOf` blockSize aes128

detectECBModeEncryption :: [Ciphertext] -> (Ciphertext, Int)
detectECBModeEncryption = maximumBy (comparing snd) . map detectRepeats
  where detectRepeats ciphertext = (ciphertext, length ciphertext - S.size (S.fromList ciphertext))
