{-# OPTIONS_GHC -Wall #-}

module System.Random.Utils (
    randomBytes
) where

import Data.Word (Word8)
import System.Random (randomRs, getStdGen)

randomBytes :: Int -> IO [Word8]
randomBytes n = do
    gen <- getStdGen
    return $ take n $ randomRs (0, 255) gen
