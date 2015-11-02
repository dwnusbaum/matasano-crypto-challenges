module Main (
    main
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Crypto.BlockCipherAttacks

main :: IO ()
main = do
    putStrLn "Cracking the encryption takes ~ 1.5 minutes when compiled with -O2."
    putStrLn "Plaintext:"
    putStrLn $ C.unpack $ B.pack $ crackECBEncryption ecbEncryptionOracleSimple
