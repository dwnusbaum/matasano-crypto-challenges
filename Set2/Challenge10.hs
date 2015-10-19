module Set2.Challenge10 (
    main
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Codec.Binary.Base64
import Crypto.AES

main :: IO ()
main = do
    file <- readFile "Set2/data/10.txt"
    let decodedFile = B.unpack $ C.pack $ decodeBase64 $ filter (/= '\n') file
    let key = B.unpack $ C.pack "YELLOW SUBMARINE"
    let iv = replicate 16 (fromIntegral 0x00)
    let decrypted = decrypt_AES128_CBC iv key decodedFile
    putStrLn $ C.unpack $ B.pack decrypted
