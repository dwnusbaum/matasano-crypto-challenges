module Set2.Challenge13 (
    main
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Crypto.AES
import Crypto.QueryString.Attack
import Crypto.QueryString.Utils

main :: IO ()
main = do
    let adminBytes = buildAdminProfile profileFor
    print $ decryptQueryString adminBytes
