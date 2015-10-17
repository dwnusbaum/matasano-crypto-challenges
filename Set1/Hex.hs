{-# OPTIONS_GHC -Wall #-}

module Hex (
    encodeHex,
    decodeHex,
    showHex
) where

import Data.Bits
import Data.Word(Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V

hexAlphabet :: String
hexAlphabet = ['0'..'9'] ++ ['a'..'f']

encodeHex :: String -> String
encodeHex = concatMap showHex . B.unpack . C.pack

decodeHex :: String -> String
decodeHex = C.unpack . B.pack . hexStringToWord8List
  where hexStringToWord8List [] = []
        hexStringToWord8List (c1 : c2 : cs) = ((hexCharToWord8 c1 `shiftL` 4) .|. hexCharToWord8 c2) : hexStringToWord8List cs
          where hexCharToWord8 :: Char -> Word8
                hexCharToWord8 c = case lookup c $ zip hexAlphabet [0..15] of
                    Just n -> fromInteger n
                    Nothing -> error $ "hexCharToWord8: Invalid character \"" ++ [c] ++ "\""
        hexStringToWord8List _ = error "Invalid hex string."

showHex :: Word8 -> String
showHex b = [alphabet V.! upperHalf b, alphabet V.! lowerHalf b]
  where upperHalf = fromIntegral . (`shiftR` 4)
        lowerHalf = fromIntegral . (.&. 0x0F)
        alphabet = V.fromList $ ['0'..'9'] ++ ['a'..'f']
