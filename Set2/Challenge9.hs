module Set2.Challenge9 (
    main
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Crypto.PKCS7

main :: IO ()
main = do
    let plaintext = "YELLOW SUBMARINE"
    print $ C.unpack $ B.pack $ padPKCS7 20 $ B.unpack $ C.pack plaintext
    putStrLn "Should be:"
    print "YELLOW SUBMARINE\x04\x04\x04\x04"

