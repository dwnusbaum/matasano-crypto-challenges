{-# OPTIONS_GHC -Wall #-}

module Crypto.QueryString.Attack (
    buildAdminProfile
) where

import Data.Word (Word8)

import Data.List.Utils

buildAdminProfile :: (String -> [Word8]) -> [Word8]
buildAdminProfile oracle = adminProfile
  where -- |email=789@12345.|com&uid=10&role=|user000000000000
        first2Blocks = take 2 (oracle "789@12345.com" `chunksOf` 16)
        -- |email=789@12345.|admin00000000000|com&uid=10&role=|user000000000000
        adminBlock = head $ take 1 $ drop 1 (oracle "789@12345.admin\0\0\0\0\0\0\0\0\0\0\0com" `chunksOf` 16)
        -- |email=789@12345.|com&uid=10&role=|admin00000000000
        adminProfile = concat first2Blocks ++ adminBlock
