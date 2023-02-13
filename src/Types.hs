module Types where

data Square = Bomb {showing :: Bool} | Flagged | Good {value :: Int, showing :: Bool} deriving (Eq)

instance Show Square where
  show (Bomb showing) = if showing then "B" else "_"
  show Flagged = "F"
  show (Good v s)
    | s = show v --if v > 0 then show v else " "
    | otherwise = "_"

data GameState = GameState
  { board :: Board,
    gameException :: GameException
  }

data GameException = None | GameOver | GameWon | InvalidMove deriving (Show, Eq)

type Row = [Square]

type Board = [Row]

type Bomb = (Int, Int)

type Bombs = [Bomb]

type Move = (Int, Int)

type Neighbor = (Int, Int)

type Neighbors = [Neighbor]