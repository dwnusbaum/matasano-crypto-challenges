{-# OPTIONS_GHC -Wall #-}

module Crypto.QueryString (
    QueryString(..),
    profileFor,
    decryptQueryString,
    encryptQueryString,
    encodeQueryString,
    parseEncodedQueryString
) where

import Control.Applicative (liftA)
import Data.List (intercalate)
import Data.Word (Word8)
import Text.Parsec (many1, parse, Parsec, ParseError, sepBy)
import Text.Parsec.Char (char, noneOf)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Codec.Binary.Base64
import Crypto.AES
import Crypto.PKCS7

import Debug.Trace

type Key = String
type Value = String
type KeyValuePair = (Key, Value)

type Parser = Parsec String ()

data QueryString = QueryString [KeyValuePair]

instance Show QueryString where
    show = encodeQueryString

profileFor :: String -> QueryString
profileFor s = QueryString [("email", sanitized), ("uid", "10"), ("role", "user")]
  where sanitized = filter (\c -> c /= '=' && c /= '&') s

secretKey :: [Word8]
secretKey = B.unpack $ C.pack $ decodeBase64 "VJAJGGV9o9eZSmJZ3PqY+Q=="

encryptQueryString :: QueryString -> [Word8]
encryptQueryString qs = encrypt_AES128_ECB secretKey plaintext
  where plaintext = padPlaintext 8 $ B.unpack $ C.pack $ encodeQueryString qs

decryptQueryString :: [Word8] -> Either ParseError QueryString
decryptQueryString bs = parseEncodedQueryString $ traceShowId plaintext
  where plaintext = C.unpack $ B.pack $ decrypt_AES128_ECB secretKey bs

-- Encodes a query string as a string
encodeQueryString :: QueryString -> String
encodeQueryString (QueryString kvs) = intercalate "&" $ map showKeyValue kvs
  where showKeyValue (k, v) = k ++ "=" ++ v

-- Parses a query string
parseEncodedQueryString :: String -> Either ParseError QueryString
parseEncodedQueryString = parse queryString "Query string"

queryString :: Parser QueryString
queryString = liftA QueryString $ keyValuePair `sepBy` char '&'

keyValuePair :: Parser KeyValuePair
keyValuePair = do
    key <- many1 $ noneOf "=&"
    _ <- char '='
    value <- many1 $ noneOf "=&"
    return (key, value)
