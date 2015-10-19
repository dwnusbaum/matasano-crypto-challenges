module Set1.Challenge6 (
    main
) where

import Codec.Binary.Base64
import Crypto.Vignerre

main :: IO ()
main = do
    file <- readFile "Set1/data/6.txt"
    let decodedFile = decodeBase64 $ filter (/= '\n') file
    let decrypted = crackRepeatingKeyXor decodedFile
    putStrLn $ fst decrypted
    putStrLn $ "KEY: " ++ snd decrypted
