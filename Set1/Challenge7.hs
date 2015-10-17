{-# OPTIONS_GHC -Wall #-}

module Challenge7 (
    main,
    aes128,
    ecb
) where

import Data.Bits
import Data.Word (Word8)
import Data.Vector (Vector, (!))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V

import Base64
import Challenge6 (chunksOf)
import RijndaelField

type Cipher = Key -> [Word8] -> [Word8]
type Key = [Word8]
type Mode = BlockCipher -> Cipher

data BlockCipher = BlockCipher {
    decrypt :: Cipher,
    encrypt :: Cipher,
    blockSize :: Int
}

aes128 :: BlockCipher
aes128 = BlockCipher encryptAES decryptAES 16

toStateArray :: [Word8] -> Vector (Vector Word8)
toStateArray xs = V.fromList $ map rows [0..3]
  where rows r = V.fromList $ map (\c -> vs ! (r + c)) [0, 4, 8, 12]
        vs = V.fromList xs

fromStateArray :: Vector (Vector Word8) -> [Word8]
fromStateArray xs = concatMap columns [0..3]
  where columns c = map (\r -> (xs ! r) ! c) [0..3]

encryptAES :: Cipher
encryptAES key = fromStateArray . cipher . toStateArray
  where cipher = undefined

decryptAES :: Cipher
decryptAES key = fromStateArray . inverseCipher . toStateArray
  where inverseCipher = undefined

ecb :: BlockCipher -> Mode -> Cipher
ecb cipher mode key input = concatMap (mode cipher key) blocks
  where blocks = input `chunksOf` blockSize cipher

main :: IO ()
main = do
    file <- readFile "data/7.txt"
    let fileBytes = B.unpack $ C.pack $ decodeBase64 $ filter (/= '\n') file
    let key =  B.unpack $ C.pack "YELLOW SUBMARINE"
    let decrypted = ecb aes128 decrypt key fileBytes
    putStrLn $ C.unpack $ B.pack decrypted
