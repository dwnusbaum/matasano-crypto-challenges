{-# OPTIONS_GHC -Wall #-}

module Challenge5 (
    main,
    repeatingKeyXor
) where

import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Hex

repeatingKeyXor :: String -> String -> String
repeatingKeyXor string key = encodeHex $ C.unpack $ B.pack $ process stringBytes repeatedKey
    where process [] _ = []
          process _ [] = error "Key should not have been shorter than input."
          process (s:ss) (k:ks) = s `xor` k : process ss ks
          repeatedKey = concat $ replicate ((length stringBytes) `div` (length keyBytes) + 1) keyBytes
          stringBytes = B.unpack $ C.pack string
          keyBytes = B.unpack $ C.pack key

main :: IO ()
main = putStrLn $ repeatingKeyXor input key
  where input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
        key = "ICE"
