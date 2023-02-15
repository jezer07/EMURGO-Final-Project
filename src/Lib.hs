{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
module Lib where

import Control.Lens
import Control.Monad.State
import Data.List (intercalate, break)
import System.Random
import Types
import Data.Char (toUpper, digitToInt)

_SIZE_OF_BOARD_ = 7

_TOTAL_SQUARES_ = _SIZE_OF_BOARD_ * _SIZE_OF_BOARD_

_TOTAL_BOMBS_ = round $ fromIntegral _TOTAL_SQUARES_ * 0.15

_RANGE_ = [0 .. _SIZE_OF_BOARD_ -1]

_HEADER_ = ' ' : formatLine (showInts _RANGE_)

_INVALID_INPUT_  = (Invalid, (-1, -1))

_SEP_ = ['_', '|', '_']

_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo:: IO ()
printLogo =  readFile _LOGO_PATH_ >>= putStrLn

formatLine :: [String] -> String
formatLine x = _SEP_ ++ intercalate _SEP_ x ++ _SEP_

convertRowIndex:: Char -> Int
convertRowIndex n = fromEnum (toUpper n) - 65

isDigit::Char -> Bool
isDigit x = x `elem` nums
    where
        nums = ['0'..'9']

readDigit:: Char -> Int
readDigit x
    | not $ isDigit x = -1
    | otherwise = digitToInt x

stringToMove:: String -> Move
stringToMove [] = (-1, -1)
stringToMove [_] = (-1, -1)
stringToMove (x:y:_) = (convertRowIndex x,  readDigit y)

stringToCommand:: String -> Command
stringToCommand [] = Invalid
stringToCommand (x:xs)
    | toUpper x == 'F' = Flag
    | toUpper x == 'O' = Open
    | otherwise = Invalid


processInput :: String -> (Command, Move)
processInput [] = _INVALID_INPUT_
processInput input = (stringToCommand command, stringToMove $ tail move)
    where
        (command, move) = break (' '==) input

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
initRow = replicate _SIZE_OF_BOARD_ Tile {value = 0, showing = False, flagged = False}

initBoard :: Bombs -> Board
initBoard b = initBombNeighbors b . plantBombs b $ replicate _SIZE_OF_BOARD_ initRow

plantBombs :: Bombs -> Board -> Board
plantBombs bombs board = foldl (\b (r, c) -> updateBoard (r, c) Bomb {showing = False, flagged = False} b) board bombs

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
    addValueToSquare (Bomb a f) = Bomb a f
    addValueToSquare (Tile v s f) = Tile (v + 1) s f


updateBoard :: Move -> Square -> Board -> Board
updateBoard (r, c) square board = board & element r . element c .~ square

revealBombs :: Board -> Bombs -> Board
revealBombs = foldr (\(r, c) b -> updateBoard (r, c) (b !! r !! c){showing = True} b)


flagSquare :: Move -> Board -> Either (GameException, Board) Board
flagSquare (r, c) board
  | not $ isWithinBounds (r, c) = Left (InvalidMove, board)
  | square == Tile (value square) True (flagged square) = Left (InvalidMove, board)
  | otherwise = Right $ updateBoard (r, c) (square {flagged = not $ flagged square}) board
  where square = board !! r !! c

openSquare:: Move -> Board ->  Either (GameException, Board) Board
openSquare (r, c) board
  | not $ isWithinBounds (r, c) = Left (InvalidMove, board)
  | square == Tile (value square) (showing square) True = Left (InvalidMove, board)
  | square == Bomb {showing = False, flagged = False} = Left (GameOver, board)
  | square == Bomb {showing = False, flagged = True} = Left (InvalidMove, board)
  | otherwise = Right $ openSquareRecursive (r, c) board
  where square = board !! r !! c

openSquareRecursive :: Move -> Board ->  Board
openSquareRecursive (r, c) board
  | square == Tile (value square) (showing square) True = board
  | square == Bomb {showing = False, flagged = False} = board
  | square == Bomb {showing = False, flagged = True} = board
  | showing square = board
  | value square > 0 = updateBoard (r, c) (square {showing = True}) board
  | otherwise =  updateBoard (r, c) (square {showing = True}) board `openNeighbors` getNeighbors (r, c)
  where square = board !! r !! c

openNeighbors:: Board -> Neighbors -> Board
openNeighbors board [] = board
openNeighbors board neighbors = foldr openSquareRecursive board neighbors
    where
        square:: Neighbor -> Square
        square n = board !! fst n !! snd n

getAllNonBombsSquares:: Board -> Bombs -> [Square]
getAllNonBombsSquares board bombs = [board !! r !! c | r <- [0 .. _SIZE_OF_BOARD_ -1], c <- [0 .. _SIZE_OF_BOARD_ -1], (r, c) `notElem` bombs]

checkIfWon:: Board -> Bombs -> Bool
checkIfWon board bombs = all ((== True) . showing) (getAllNonBombsSquares board bombs)

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