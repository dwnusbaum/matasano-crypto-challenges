module Crypto.BlockCipher (
    detectECBModeEncryption,
    detectECBorCBC,
    encryptECBorCBC
) where

import Data.Bits
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import System.Random

import qualified Data.Set as S

import Crypto.AES

type Ciphertext = [String]

data BlockMode = ECB | CBC
  deriving (Show)

detectECBModeEncryption :: [Ciphertext] -> (Ciphertext, Int)
detectECBModeEncryption = maximumBy (comparing snd) . map detectRepeats
  where detectRepeats ciphertext = (ciphertext, length ciphertext - S.size (S.fromList ciphertext))

detectECBorCBC :: [Word8] -> BlockMode
detectECBorCBC = undefined

encryptECBorCBC :: [Word8] -> IO [Word8]
encryptECBorCBC bytes = do
    key <- randomBytes 16
    mode <- randomRIO (0, 1) :: IO Int
    case mode of
        0 -> return $ encrypt_AES128_ECB key bytes
        1 -> do
            iv <- randomBytes 16
            return $ encrypt_AES128_CBC iv key bytes

randomBytes :: Int -> IO [Word8]
randomBytes n = do
    gen <- getStdGen
    return $ take n $ randomRs (0, 255) gen
