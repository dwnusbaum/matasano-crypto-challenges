{-# OPTIONS_GHC -Wall #-}

module Base64 (
    encodeBase64,
    decodeBase64
) where

import Data.Bits
import Data.Maybe (fromJust, fromMaybe)
import Data.Word(Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

base64Alphabet :: String
base64Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

encodeBase64 :: String -> String
encodeBase64 = word8ListToBase64String . B.unpack . C.pack
  where word8ListToBase64String [] = []
        word8ListToBase64String ws = case length ws of
            1 -> take 2 (encodeTriplet [head ws, 0, 0]) ++ "=="
            2 -> take 3 (encodeTriplet (take 2 ws ++ [0])) ++ "="
            _ -> encodeTriplet (take 3 ws) ++ word8ListToBase64String (drop 3 ws)

        encodeTriplet :: [Word8] -> String
        encodeTriplet [a, b, c] = map word8Tobase64Char [first, second, third, fourth]
            where first = a `shiftR` 2
                  second = clearBit (clearBit (a `shiftL` 4) 7) 6 .|. (b `shiftR` 4)
                  third = clearBit (clearBit (b `shiftL` 2) 7) 6 .|. (c `shiftR` 6)
                  fourth = clearBit (clearBit c 7) 6
                  word8Tobase64Char x = fromJust $ lookup x $ zip [0..63] base64Alphabet
        encodeTriplet _ = error "encodeTriplet: Group had less than 3 items."

decodeBase64 :: String -> String
decodeBase64 = C.unpack . B.pack . base64StringToWord8List . takeWhile (/= '=')
  where base64StringToWord8List [] = []
        base64StringToWord8List ws = case length ws of
            1 -> error "Not possible"
            2 -> take 2 $ decodeQuartet $ map base64CharToWord8 $ take 2 ws ++ "AA"
            3 -> take 3 $ decodeQuartet $ map base64CharToWord8 $ take 3 ws ++ "A"
            _ -> decodeQuartet (map base64CharToWord8 $ take 4 ws) ++ base64StringToWord8List (drop 4 ws)
          where base64CharToWord8 x = fromMaybe (error $ show x) (lookup x $ zip base64Alphabet [0..63])

        decodeQuartet :: [Word8] -> [Word8]
        decodeQuartet [a, b, c, d] = [first, second, third]
          where first = a `shiftL` 2 .|. b `shiftR` 4
                second = b `shiftL` 4 .|. c `shiftR` 2
                third = c `shiftL` 6 .|. d
        decodeQuartet _ = error "decodeQuartet: Group had less than 4 items."
