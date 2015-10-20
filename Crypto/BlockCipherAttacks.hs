{-# OPTIONS_GHC -Wall #-}

module Crypto.BlockCipherAttacks (
    findECBModeEncryption,
    detectECBorCBC,
    encryptECBorCBC,
    ecbEncryptionOracle,
    findBlockSize
) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import System.Random (randomRIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S

import Codec.Binary.Base64
import Crypto.AES
import Crypto.PKCS7
import Data.List.Utils
import System.Random.Utils

type Ciphertext = [Word8]
type CiphertextBlocks = [Ciphertext]

data BlockMode = ECB | CBC
  deriving (Show)

findECBModeEncryption :: [Ciphertext] -> (Ciphertext, Int)
findECBModeEncryption cs = maximumBy (comparing snd) repeated
    where repeated = map (\c -> (c, repeatedBlocks $ c `chunksOf` 16)) cs

repeatedBlocks :: CiphertextBlocks -> Int
repeatedBlocks blocks = length blocks - S.size (S.fromList blocks)

detectECBorCBC :: Ciphertext -> BlockMode
detectECBorCBC bytes = if detectECBModeEncryption bytes
    then ECB
    else CBC
  where detectECBModeEncryption = (> 0) . repeatedBlocks . (`chunksOf` 16)

encryptECBorCBC :: String -> IO Ciphertext
encryptECBorCBC plaintext = do
    key <- randomBytes 16
    bytes' <- randomPaddingPlaintext
    mode <- randomRIO (0, 1) :: IO Int
    case mode of
        0 -> do
            putStrLn "The encryption oracle chose to encrypt using ECB."
            return $ encrypt_AES128_ECB key bytes'
        1 -> do
            putStrLn "The encryption oracle chose to encrypt using CBC."
            iv <- randomBytes 16
            return $ encrypt_AES128_CBC iv key bytes'
        _ -> error "BlockCipher.hs: The random number wasn't 0 or 1!"
  where randomPaddingPlaintext = do
            prefix <- randomRIO (5, 10) >>= randomBytes
            suffix <- randomRIO (5, 10) >>= randomBytes
            return $ padPlaintext 16 $ prefix ++ B.unpack (C.pack plaintext) ++ suffix

ecbEncryptionOracle :: [Word8] -> Ciphertext
ecbEncryptionOracle input = encrypt_AES128_ECB key plaintext
  where plaintext = padPlaintext 16 $ input ++ secret
        secret = B.unpack $ C.pack $ decodeBase64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
        key = B.unpack $ C.pack $ decodeBase64 "VJAJGGV9o9eZSmJZ3PqY+Q=="

findBlockSize :: ([Word8] -> Ciphertext) -> Int
findBlockSize oracle = go 1
  where go n
          | take n ciphertext == take n (drop n ciphertext) = n `div` 2
          | otherwise = go (n + 1)
          where ciphertext = oracle $ replicate n 0x20
