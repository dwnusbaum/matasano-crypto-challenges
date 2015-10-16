module Challenge7 (
    main,
    aes128,
    ecb
) where

import Data.Bits
import Data.List (minimumBy, sortBy, transpose)
import Data.Ord (comparing)
import Data.Word (Word8, Word32)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Challenge6 (chunksOf)

import Base64

type BlockSize = Int
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

encryptAES :: Cipher
encryptAES key block = undefined

decryptAES :: Cipher
decryptAES key block = undefined

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
