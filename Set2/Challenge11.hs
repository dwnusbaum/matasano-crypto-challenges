module Set2.Challenge11 (
    main
) where

import Codec.Binary.Base64
import Crypto.BlockCipher

main :: IO ()
main = do
    plaintext <- readFile "Set2/data/11.txt"
    let plaintext' = decodeBase64 plaintext
    encrypted <- encryptECBorCBC plaintext'
    let detected = detectECBorCBC encrypted
    putStrLn $ "The detection oracle thinks that the ciphertext was encrypted using " ++ show detected
