{-# LANGUAGE FlexibleContexts #-}

module Actions where
import Lib
import Types
import Control.Monad.State  ( MonadIO, MonadState, get, gets, modify, put, evalStateT, liftIO, when )


startGame :: IO ()
startGame = do
  x <- generateRandomList
  y <- generateRandomList
  let bombs = removeDuplicates (getBombsIndexes x y)
  let board = initBoard bombs
  printLogo
  evalStateT playGameLoop (GameState board bombs True)

playGameLoop :: (MonadIO m, MonadState GameState m) => m ()
playGameLoop = do
  GameState board bombs stillPlaying <- get
  liftIO $ printBoard board
  when stillPlaying $
    do
      move <- liftIO promptMove
      case fst move of
        Flag -> do
          let result = flagSquare (snd move) board
          case result of
            Left (e,b) -> do
              liftIO $ print e
              playGameLoop
            Right newBoard -> do
              put (GameState newBoard bombs stillPlaying)
              playGameLoop
        Open -> do
          let result = openSquare (snd move) board
          case result of
            Left (e,b) -> do
              if e == GameOver then do
                  put (GameState b bombs False)
                  liftIO $ printBoard $ revealBombs b bombs
                  liftIO $ print e
              else
                  playGameLoop
            Right newBoard -> do
              put (GameState newBoard bombs stillPlaying)
              if checkIfWon newBoard bombs then do
                  liftIO $ printBoard newBoard
                  liftIO $ putStrLn "====================== Congratulations You've Won!!! ======================"
                  put (GameState newBoard bombs False)
              else
                playGameLoop
        Invalid -> do
          liftIO $ putStrLn "Invalid command"
          playGameLoop


printBoard:: Board -> IO ()
printBoard = putStrLn . formatBoard

promptMove :: IO (Command, Move)
promptMove = do
  putStrLn "Commands: F to flag, O to open"
  putStrLn "Enter a move: <command><space><coordinate>"
  processInput <$> getLine
