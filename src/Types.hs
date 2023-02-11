module Types where

data Square = B | F | E deriving (Show, Eq)

type Row = [Square]
type Board = [Row]



