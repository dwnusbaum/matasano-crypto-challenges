module Main (
    main
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Codec.Binary.Base64
import Crypto.BlockCipherAttacks

main :: IO ()
main = putStrLn $ C.unpack $ B.pack $ crackECBEncryption ecbEncryptionOracle
