module Set1.Challenge8 (
    main
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Codec.Binary.Hex
import Crypto.BlockCipherAttacks
import Data.List.Utils

-- Fix this
main :: IO ()
main = do
    file <- readFile "Set1/data/8.txt"
    let ciphertexts = map (B.unpack . C.pack . decodeHex) $ lines file
    let detected = findECBModeEncryption ciphertexts
    putStrLn $ concatMap showHex $ fst detected
    putStrLn $ show (snd detected) ++ " blocks were repeated."
