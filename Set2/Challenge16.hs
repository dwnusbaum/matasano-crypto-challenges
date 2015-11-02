module Set2.Challenge16 (
    main
) where

import Crypto.BlockCipherAttacks

main :: IO ()
main = do
    let adminCiphertext = makeAdminBlock cbcEncryptionOracle
    print $ containsAdminBlock adminCiphertext
