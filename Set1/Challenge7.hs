module Set1.Challenge7 (
    main,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Codec.Binary.Base64
import Crypto.AES

main :: IO ()
main = do
    file <- readFile "Set1/data/7.txt"
    let fileBytes = B.unpack $ C.pack $ decodeBase64 $ filter (/= '\n') file
    let key =  B.unpack $ C.pack "YELLOW SUBMARINE"
    let decrypted = aes128ecb decrypt key fileBytes
    putStrLn $ C.unpack $ B.pack decrypted
