module Set2.Challenge15 (
    main
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Crypto.PKCS7

main :: IO ()
main = do
    let plaintext1 = "ICE ICE BABY\x04\x04\x04\x04"
    case validatePKCS7 $ B.unpack $ C.pack plaintext1 of
        Left () -> error $ show plaintext1 ++ " should have been padded correctly!"
        Right v -> putStrLn $ show plaintext1 ++ " is padded correctly: " ++ show (C.unpack (B.pack v))
    let plaintext2 = "ICE ICE BABY\x05\x05\x05\x05"
    case validatePKCS7 $ B.unpack $ C.pack plaintext2 of
        Left () -> putStrLn $ show plaintext1 ++ " is padded incorrectly."
        Right v -> error $ show plaintext1 ++ " should not have been padded correctly!" ++ show (C.unpack (B.pack v))
    let plaintext3 = "ICE ICE BABY\x01\x02\x03\x04"
    case validatePKCS7 $ B.unpack $ C.pack plaintext3 of
        Left () -> putStrLn $ show plaintext1 ++ " is padded incorrectly."
        Right v -> error $ show plaintext1 ++ " should not have been padded correctly!" ++ show (C.unpack (B.pack v))

