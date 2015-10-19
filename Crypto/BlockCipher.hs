{-# OPTIONS_GHC -Wall #-}

module Crypto.BlockCipher (
    findECBModeEncryption,
    detectECBorCBC,
    encryptECBorCBC
) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import System.Random

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S

import Crypto.AES
import Crypto.PKCS7
import Data.List.Utils

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
            putStrLn "The encryption oracele chose to encrypt using ECB."
            return $ encrypt_AES128_ECB key bytes'
        1 -> do
            putStrLn "The encryption oracele chose to encrypt using CBC."
            iv <- randomBytes 16
            return $ encrypt_AES128_CBC iv key bytes'
        _ -> error "BlockCipher.hs: The random number wasn't 0 or 1!"
  where randomPaddingPlaintext = do
            prefixLength <- randomRIO (5, 10)
            suffixLength <- randomRIO (5, 10)
            prefix <- randomBytes prefixLength
            suffix <- randomBytes suffixLength
            return $ padPlaintext 16 $ prefix ++ B.unpack (C.pack plaintext) ++ suffix

randomBytes :: Int -> IO [Word8]
randomBytes n = do
    gen <- getStdGen
    return $ take n $ randomRs (0, 255) gen
