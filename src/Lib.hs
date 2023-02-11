module Lib where

import System.Random
import Types

_SIZE_OF_BOARD_ = 7

_TOTAL_SQUARES_ = _SIZE_OF_BOARD_ * _SIZE_OF_BOARD_

_TOTAL_BOMBS_ =  round $ fromIntegral _TOTAL_SQUARES_ *  0.3

generateRandomList :: IO [Int]
generateRandomList  = do
  newStdGen
  take _TOTAL_BOMBS_ . randomRs (0, _SIZE_OF_BOARD_ - 1) <$> getStdGen

getBombsIndexes:: [Int] -> [Int] -> [(Int, Int)]
getBombsIndexes = zip 

generateEmptyRow:: Row
generateEmptyRow = replicate _SIZE_OF_BOARD_ E        

generateEmptyBoard :: Board
generateEmptyBoard = replicate _SIZE_OF_BOARD_ generateEmptyRow

fillBoardWithBombs:: Board -> Board
fillBoardWithBombs = 

printRandomList :: IO ()
printRandomList = do
    list <- generateRandomList
    print list
