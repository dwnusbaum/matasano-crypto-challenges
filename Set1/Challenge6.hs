module Challenge6 (
    main,
    hammingDistance
) where

import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

hammingDistance :: String -> String -> Int
hammingDistance a b = sum $ map popCount $ zipWith xor aBytes bBytes
  where aBytes = B.unpack $ C.pack a
        bBytes = B.unpack $ C.pack b

main :: IO ()
main = print $ hammingDistance input1 input2
  where input1 = "this is a test"
        input2 = "wokka wokka!!!"
