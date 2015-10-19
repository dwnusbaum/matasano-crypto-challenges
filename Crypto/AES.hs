{-# OPTIONS_GHC -Wall #-}

module Crypto.AES (
    BlockCipher(..),
    aes128ecb,
    detectECBModeEncryption
) where

import Data.List (maximumBy)
import Data.Ord (comparing)

import qualified Data.Set as S

import Crypto.AES.Cipher
import Data.List.Utils

type Action = BlockCipher -> Cipher
type Ciphertext = [String]

ecb :: BlockCipher -> Action -> Cipher
ecb cipher mode key input = concatMap (mode cipher key) blocks
  where blocks = input `chunksOf` blockSize cipher

aes128ecb :: Action -> Cipher
aes128ecb = ecb aes128

detectECBModeEncryption :: [Ciphertext] -> (Ciphertext, Int)
detectECBModeEncryption = maximumBy (comparing snd) . map detectRepeats
  where detectRepeats ciphertext = (ciphertext, length ciphertext - S.size (S.fromList ciphertext))
