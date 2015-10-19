module Set1.Challenge8 (
    main
) where

import Crypto.AES
import Data.List.Utils

main :: IO ()
main = do
    file <- readFile "Set1/data/8.txt"
    let ciphertexts = lines file
    let detected = detectECBModeEncryption $ map (`chunksOf` 32) ciphertexts -- 32 hex chars are 16 bytes = 1 block
    putStrLn $ concat $ fst detected
    putStrLn $ show (snd detected) ++ " blocks were repeated."
