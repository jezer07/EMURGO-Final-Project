module Lib where


import Control.Monad
import Control.Monad.State
import System.Random
import Types

_SIZE_OF_BOARD_ = 7

_TOTAL_SQUARES_ = _SIZE_OF_BOARD_ * _SIZE_OF_BOARD_

_TOTAL_BOMBS_ = round $ fromIntegral _TOTAL_SQUARES_ * 0.3

generateRandomList :: IO [Int]
generateRandomList = do
  newStdGen
  take _TOTAL_BOMBS_ . randomRs (0, _SIZE_OF_BOARD_ - 1) <$> getStdGen

getBombsIndexes :: [Int] -> [Int] -> Bombs
getBombsIndexes = zip

initRow :: Row
initRow = replicate _SIZE_OF_BOARD_ E

initBoard :: Board
initBoard = replicate _SIZE_OF_BOARD_ initRow

printBoard :: Board -> IO ()
printBoard b = do
  list <- generateRandomList
  print list
