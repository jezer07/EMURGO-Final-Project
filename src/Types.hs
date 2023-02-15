module Types where

data Square = Bomb {showing :: Bool, flagged:: Bool} | Tile {value :: Int, showing :: Bool, flagged:: Bool} deriving (Eq)

data Command = Flag | Open | Invalid deriving (Show, Eq)



data GameState = GameState
  { 
    board :: Board,
    bombs :: Bombs,
    stillPlaying :: Bool
  }

data GameException = None | GameOver | InvalidMove deriving (Eq)

type Row = [Square]

type Board = [Row]

type Bomb = (Int, Int)

type Bombs = [Bomb]

type Move = (Int, Int)

type Neighbor = (Int, Int)

type Neighbors = [Neighbor]

instance Show Square where
  show (Bomb showing flagged) 
    | showing = "B"
    | flagged = "F"
    | otherwise = "_"

  show (Tile v s f)
    | s = show v --if v > 0 then show v else " "
    | f = "F"
    | otherwise = "_"

instance Show GameException where
  show None = "None"
  show GameOver = "You've opened up a Bomb! Game over!"
  show InvalidMove = "Invalid move"