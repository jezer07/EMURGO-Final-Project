module Actions where
import Lib
import Types

startGame :: IO ()
startGame = do
  x <- generateRandomList
  y <- generateRandomList
  let bombs = removeDuplicates (getBombsIndexes x y)
  let board = initBoard bombs
  printBoard board
  let newBoard = openSquare (3,3) board
  printBoard newBoard
                                 

printBoard:: Board -> IO ()
printBoard = putStrLn . formatBoard 