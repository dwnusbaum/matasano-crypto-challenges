module Set2.Challenge9 (
    main
) where

import Crypto.PKCS7

main :: IO ()
main = do
    print $ padPlaintext 20 "YELLOW SUBMARINE"
    putStrLn "Should be:"
    print "YELLOW SUBMARINE\x04\x04\x04\x04"

