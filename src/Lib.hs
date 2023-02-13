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
initRow = replicate _SIZE_OF_BOARD_ Good {value = 0, showing = False}

initBoard :: Bombs -> Board
initBoard b = initBombNeighbors b . plantBombs b $ replicate _SIZE_OF_BOARD_ initRow

plantBombs :: Bombs -> Board -> Board
plantBombs bombs board = foldl (\b (r, c) -> updateBoard (r, c) Bomb {showing = False} b) board bombs

initBombNeighbors :: Bombs -> Board -> Board
initBombNeighbors [] board = board
initBombNeighbors bombs board = foldr updateNeighbor board (concatMap getNeighbors bombs)
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

updateBoard :: Move -> Square -> Board -> Board
updateBoard (r, c) square board = board & element r . element c .~ square

revealBombs :: Board -> Bombs -> Board
revealBombs = foldl (\b (r, c) -> updateBoard (r, c) Bomb {showing = False} b)

openSquare :: Move -> Board ->  Board
openSquare (r, c) board
  | square == Flagged = board
  | square == Bomb {showing = True} = board
  | showing square = board
  | value square > 0 = updateBoard (r, c) (square {showing = True}) board 
  | otherwise =  updateBoard (r, c) (square {showing = True}) board `openNeighbors` getNeighbors (r, c)
  where square = board !! r !! c

openNeighbors:: Board -> Neighbors -> Board
openNeighbors board [] = board
openNeighbors board neighbors = foldr (\n b -> openSquare (fst n, snd n) b) board neighbors
    where
        square:: Neighbor -> Square
        square n = board !! fst n !! snd n


isValidSquareToOpenAutomatically:: Square -> Bool
isValidSquareToOpenAutomatically square = square /= Flagged && square /= Bomb {showing = True} && square /= Bomb {showing = False}


isWithinBounds:: Move -> Bool
isWithinBounds (r, c) = r >= 0 && r < _SIZE_OF_BOARD_ && c >= 0 && c < _SIZE_OF_BOARD_

getNeighbors:: Neighbor -> Neighbors
getNeighbors (r, c) =
  filter
    (\(r, c) -> isWithinBounds (r, c))
    [ (r - 1, c - 1),
      (r - 1, c),
      (r - 1, c + 1),
      (r, c - 1),
      (r, c + 1),
      (r + 1, c - 1),
      (r + 1, c),
      (r + 1, c + 1)
    ]