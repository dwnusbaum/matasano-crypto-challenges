{-# OPTIONS_GHC -Wall #-}

module Crypto.BlockCipherAttacks (
    findECBModeEncryption,
    detectECBorCBC,
    encryptECBorCBC,
    crackECBEncryption,
    ecbEncryptionOracleSimple,
    ecbEncryptionOracleHard,
    cbcEncryptionOracle,
    containsAdminBlock,
    makeAdminBlock,
) where

import Data.Bits (xor)
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
            return $ padPKCS7 16 $ prefix ++ B.unpack (C.pack plaintext) ++ suffix

ecbEncryptionOracleSimple :: [Word8] -> Ciphertext
ecbEncryptionOracleSimple input = encrypt_AES128_ECB secretKey plaintext
  where plaintext = input ++ secret

ecbEncryptionOracleHard :: [Word8] -> Ciphertext
ecbEncryptionOracleHard input = encrypt_AES128_ECB secretKey plaintext
  where plaintext = secretPrefix ++ input ++ secret

secretPrefix :: [Word8]
secretPrefix = [0x01, 0xc5, 0x06, 0xa7, 0xb2, 0x31, 0x7d, 0x48, 0xee]

secret :: [Word8]
secret = B.unpack $ C.pack $ decodeBase64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

secretKey :: [Word8]
secretKey = B.unpack $ C.pack $ decodeBase64 "VJAJGGV9o9eZSmJZ3PqY+Q=="

findBlockSize :: ([Word8] -> Ciphertext) -> Int
findBlockSize oracle = go 1
  where go n
          | take n ciphertext == take n (drop n ciphertext) = n
          | otherwise = go (n + 1)
          where ciphertext = drop (minPrefixLength + n) $ oracle $ replicate (n * 3) 0x20
        minPrefixLength = length $ takeWhile id $ zipWith (==) firstAttempt secondAttempt
          where firstAttempt = oracle [0x01]
                secondAttempt = oracle [0x02]

-- Needs to be tweaked for prefix greater than 1 block in length
findPrefixLength :: ([Word8] -> Ciphertext) -> Int -> Int
findPrefixLength oracle detectedBlockSize = go 1
  where go n
          | blocksMatch = detectedBlockSize - n
          | otherwise = go (n + 1)
          where ciphertext = oracle $ replicate (n + detectedBlockSize * 2) 0x20
                (first, rest) = splitAt detectedBlockSize $ drop detectedBlockSize ciphertext
                blocksMatch = first == take detectedBlockSize rest

crackECBEncryption :: ([Word8] -> Ciphertext) -> [Word8]
crackECBEncryption oracle = case detectECBorCBC $ oracle $ replicate 100 0x20 of
    ECB -> go 1 (detectedBlockSize - 1) []
    CBC -> error "BlockCipherAttacks.hs: Oracle was using CBC encryption"
  where go bi (-1) ps
          | length ps + prefixLength == unknownLength = ps -- Decrypted whole string
          | otherwise = go (bi + 1) (detectedBlockSize - 1) ps -- Decrypt next block
        go bi n ps = case correctPlaintext of
            [] -> ps -- Nothing matched, so we modified the padding or we messed up
            (x:_)  -> go bi (n - 1) $ drop (n + prependLength) x
          where ciphertext = relevant $ oracle firstBlock
                firstBlock = replicate (n + prependLength) 0x20
                lastByteDictionary = map (\b -> firstBlock ++ ps ++ [b]) [0..255]
                correctPlaintext = filter (\x -> relevant (oracle x) == ciphertext) lastByteDictionary
                relevant = take (prefixLength + prependLength + bi * detectedBlockSize)
        detectedBlockSize = findBlockSize oracle
        prefixLength = findPrefixLength oracle detectedBlockSize
        prependLength = (detectedBlockSize - prefixLength) `mod` detectedBlockSize
        unknownLength = length $ oracle []

cbcAdminIV :: [Word8]
cbcAdminIV = [0x53, 0xbf, 0x32, 0xd1, 0x05, 0xae, 0xc2, 0x40, 0x51, 0xf6, 0x77, 0x84, 0x0f, 0x4d, 0x03, 0xc6]

cbcEncryptionOracle :: [Word8] -> Ciphertext
cbcEncryptionOracle input = encrypt_AES128_CBC cbcAdminIV secretKey $ padPKCS7 16 $ prefix ++ sanitized ++ suffix
  where prefix = B.unpack $ C.pack "comment1=cooking%20MCs;userdata="
        suffix = B.unpack $ C.pack ";comment2=%20like%20a%20pound%20of%20bacon"
        sanitized = filter (\x -> x /= 0x3b && x /= 0x3d) input

containsAdminBlock :: Ciphertext -> Bool
containsAdminBlock ciphertext = case validatePKCS7 plaintext of
    Left () -> False
    Right pt -> elem "admin=true" $ C.unpack (B.pack pt) `splitOn` ';'
  where plaintext = decrypt_AES128_CBC cbcAdminIV secretKey ciphertext

makeAdminBlock :: ([Word8] -> Ciphertext) -> Ciphertext
makeAdminBlock oracle = concat (take 2 ciphertextBlocks) ++ scrambled ++ concat (drop 3 ciphertextBlocks)
  where ciphertextBlocks = oracle (B.unpack $ C.pack "BBBBBBBBBBBBBBBB:admin<true:BBBB") `chunksOf` 16
        scrambled = (head tb `xor` 0x01) : bytes1To5 ++ [tb !! 6 `xor` 0x01] ++ bytes7To10 ++ [tb !! 11 `xor` 0x01] ++ bytes12To15
          where tb = ciphertextBlocks !! 2
                bytes1To5 = take 5 $ drop 1 tb
                bytes7To10 = take 4 $ drop 7 tb
                bytes12To15 = drop 12 tb
  -- ________________ ________________ ________________ ________________
  -- comment1=cooking %20MCs;userdata= BBBBBBBBBBBBBBBB :admin<true;BBBB

