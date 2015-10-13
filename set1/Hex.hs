{-# OPTIONS_GHC -Wall #-}

module Hex (
    encodeHex,
    decodeHex,
) where

import Data.Bits
import Data.Maybe (fromJust)
import Data.Word(Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

hexAlphabet :: String
hexAlphabet = ['0'..'9'] ++ ['a'..'f']

encodeHex :: String -> String
encodeHex = concatMap word8ToHexString . B.unpack . C.pack
  where word8ToHexString w = [word8ToHexChar upperHalf, word8ToHexChar lowerHalf]
          where lowerHalf = w .&. 0x0F
                upperHalf = (w .&. 0xF0) `shiftR` 4
                word8ToHexChar x = fromJust $ lookup x $ zip [0..15] hexAlphabet

decodeHex :: String -> String
decodeHex = C.unpack . B.pack . hexStringToWord8List
  where hexStringToWord8List [] = []
        hexStringToWord8List (c1 : c2 : cs) = ((hexCharToWord8 c1 `shiftL` 4) .|. hexCharToWord8 c2) : hexStringToWord8List cs
          where hexCharToWord8 :: Char -> Word8
                hexCharToWord8 c = case lookup c $ zip hexAlphabet [0..15] of
                    Just n -> fromInteger n
                    Nothing -> error $ "hexCharToWord8: Invalid character \"" ++ [c] ++ "\""
        hexStringToWord8List _ = error "Invalid hex string."
