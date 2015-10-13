module Challenge1 (
    main
) where

import Hex
import Base64

hexInput :: String
hexInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

base64Output :: String
base64Output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

main :: IO ()
main = print $ result == base64Output
  where result = encodeBase64 $ decodeHex hexInput
