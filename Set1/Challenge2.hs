module Set1.Challenge1 (
    main
) where

import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Codec.Binary.Hex

hexstring1 :: String
hexstring1 = "1c0111001f010100061a024b53535009181c"

hexstring2 :: String
hexstring2 = "686974207468652062756c6c277320657965"

expectedHex :: String
expectedHex = "746865206b696420646f6e277420706c6179"

main :: IO ()
main = do
    putStrLn result
    putStrLn "Should match:"
    putStrLn expectedHex
  where bytes1 = B.unpack $ C.pack $ decodeHex hexstring1
        bytes2 = B.unpack $ C.pack $ decodeHex hexstring2
        result = encodeHex $ C.unpack $ B.pack $ zipWith xor bytes1 bytes2
