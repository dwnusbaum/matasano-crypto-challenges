module Challenge8 (
    main
) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as S

import Challenge6 (chunksOf)

type Ciphertext = [String]

detectECBModeEncryption :: [Ciphertext] -> (Ciphertext, Int)
detectECBModeEncryption = maximumBy (comparing snd) . map detectRepeats
  where detectRepeats ciphertext = (ciphertext, length ciphertext - S.size (S.fromList ciphertext))

main :: IO ()
main = do
    file <- readFile "data/8.txt"
    let ciphertexts = lines file
    print $ detectECBModeEncryption $ map (`chunksOf` 32) ciphertexts -- 32 hex chars are 16 bytes = 1 block

