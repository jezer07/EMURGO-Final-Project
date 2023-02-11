module Lib where

import System.Random

_SIZE_OF_BOARD = 7

generateRandomList :: Int -> IO [Int]
generateRandomList n = do
  gen <- getStdGen
  return $ take n $ randomRs (0, _SIZE_OF_BOARD - 1) gen


  