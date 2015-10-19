module Challenge4 (
    main
) where

import Codec.Binary.Hex
import Crypto.SingleByteXor

main :: IO ()
main = do
    file <- readFile "Set1/data/4.txt"
    print $ detectSingleByteXor $ map decodeHex (lines file)
