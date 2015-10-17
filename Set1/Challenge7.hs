{-# OPTIONS_GHC -Wall #-}

module Challenge7 (
    main,
    ecb
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import AES
import Base64
import Challenge6 (chunksOf)

ecb :: BlockCipher -> CipherMode -> Cipher
ecb cipher mode key input = concatMap (mode cipher key) blocks
  where blocks = input `chunksOf` blockSize cipher

main :: IO ()
main = do
    file <- readFile "data/7.txt"
    let fileBytes = B.unpack $ C.pack $ decodeBase64 $ filter (/= '\n') file
    let key =  B.unpack $ C.pack "YELLOW SUBMARINE"
    let decrypted = ecb aes128 decrypt key fileBytes
    putStrLn $ C.unpack $ B.pack decrypted
