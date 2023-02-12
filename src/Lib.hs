module Lib where

import Control.Lens
import Control.Monad.State
import Data.List (intercalate)
import System.Random
import Types

_SIZE_OF_BOARD_ = 7

_TOTAL_SQUARES_ = _SIZE_OF_BOARD_ * _SIZE_OF_BOARD_

_TOTAL_BOMBS_ = round $ fromIntegral _TOTAL_SQUARES_ * 0.15

_RANGE_ = [0 .. _SIZE_OF_BOARD_ -1]

_HEADER_ = ' ' : formatLine (showInts _RANGE_)

_SEP_ = ['_', '|', '_']

formatLine :: [String] -> String
formatLine x = _SEP_ ++ intercalate _SEP_ x ++ _SEP_

showInts :: [Int] -> [String]
showInts = map show

showSquares :: [Square] -> [String]
showSquares = map show

formatRows :: [Row] -> [String]
formatRows = map $ formatLine . showSquares

prependRowIndices :: [String] -> [String]
prependRowIndices = zipWith (:) ['A' ..]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : filter (/= x) (removeDuplicates xs)

formatBoard :: Board -> String
formatBoard = unlines . (_HEADER_ :) . prependRowIndices . formatRows

generateRandomList :: IO [Int]
generateRandomList = do
  newStdGen
  take _TOTAL_BOMBS_ . randomRs (0, _SIZE_OF_BOARD_ - 1) <$> getStdGen

getBombsIndexes :: [Int] -> [Int] -> Bombs
getBombsIndexes = zip

initRow :: Row
initRow = replicate _SIZE_OF_BOARD_ Good {value = 0, showing = True}

initBoard :: Bombs -> Board
initBoard b = initBombNeighbors b . plantBombs b $ replicate _SIZE_OF_BOARD_ initRow

plantBombs :: Bombs -> Board -> Board
plantBombs bombs board = foldl (\b (r, c) -> updateBoard (r, c) Bomb {showing = True} b) board bombs

initBombNeighbors :: Bombs -> Board -> Board
initBombNeighbors [] board = board
initBombNeighbors bombs board = foldr updateNeighbor board (concatMap getBombsNeighbors bombs)
  where
    updateNeighbor :: (Int, Int) -> Board -> Board
    updateNeighbor (r, c) board =
      if r < 0 || r >= _SIZE_OF_BOARD_ || c < 0 || c >= _SIZE_OF_BOARD_
        then board
        else updateBoard (r, c) (addValueToSquare (board !! r !! c)) board

    addValueToSquare :: Square -> Square
    addValueToSquare (Bomb a) = Bomb a
    addValueToSquare (Good v s) = Good (v + 1) s
    addValueToSquare Flagged = Flagged

    getBombsNeighbors :: Bomb -> [(Int, Int)]
    getBombsNeighbors (r, c) =
      filter
        (\(r, c) -> r >= 0 && r < _SIZE_OF_BOARD_ && c >= 0 && c < _SIZE_OF_BOARD_)
        [ (r - 1, c - 1),
          (r - 1, c),
          (r - 1, c + 1),
          (r, c - 1),
          (r, c + 1),
          (r + 1, c - 1),
          (r + 1, c),
          (r + 1, c + 1)
        ]

updateBoard :: Move -> Square -> Board -> Board
updateBoard (r, c) square board = board & element r . element c .~ square

flagSquare :: Move -> Bombs -> Board -> Either (Board, GameException) Board
flagSquare (r, c) bombs board =
  if board !! r !! c == Good {value = 0, showing = False}
    then Right (updateBoard (r, c) Flagged board)
    else Left (updateBoard (r, c) Flagged board, InvalidMove)

revealBombs :: Board -> Bombs -> Board
revealBombs = foldl (\b (r, c) -> updateBoard (r, c) Bomb {showing = False} b)