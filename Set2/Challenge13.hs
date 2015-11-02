module Set2.Challenge13 (
    main
) where

import Crypto.QueryString.Attack
import Crypto.QueryString.Utils

main :: IO ()
main = do
    let adminBytes = buildAdminProfile profileFor
    case decryptQueryString adminBytes of
        Left err -> print err
        Right profile -> print profile
