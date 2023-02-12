module Types where

data Square = B | F | E deriving (Show, Eq)

data GameState = GameState
  { board :: Board,
    bombs :: Bombs,
    isGameOver :: Bool,
    score :: Int,
    bombFlaggedCount :: Int
  }

type Row = [Square]

type Board = [Row]

type Bombs = [(Int, Int)]

type Move = (Int, Int)