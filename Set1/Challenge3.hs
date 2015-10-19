module Challenge3 (
    main,
) where

import Codec.Binary.Hex
import Crypto.SingleByteXor

hexstring :: String
hexstring = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

main :: IO ()
main = print $ crackSingleByteXor $ decodeHex hexstring
