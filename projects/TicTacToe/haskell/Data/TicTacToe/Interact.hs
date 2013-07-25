-- | Play tic-tac-toe interactively.
module Data.TicTacToe.Interact
(
  tictactoe
) where

import Data.TicTacToe.Board
import Data.TicTacToe.Position
import Data.TicTacToe.Player
import Data.TicTacToe.GameResult
import Data.Char
import Control.Monad

-- | Play tic-tac-toe interactively.
tictactoe ::
  IO ()
tictactoe =
  gameLoop (\b' -> do surround $ printWithoutPositions b'
                      tictactoe' b') empty

gameLoop ::
  (BoardLike b, Move b to) =>
  (to -> IO ())
  -> b
  -> IO ()
gameLoop k b =
  let p = whoseTurn b
  in do putStrLns
          [
            show p ++ " to move [" ++ [toSymbol p] ++ "]"
          , "  [1-9] to Move"
          , "  q to Quit"
          , "  v to view board positions"
          ]
        putStr "  > "
        c <- getChar
        if c `elem` "vV"
          then
            do surround $ printWithPositions b
               gameLoop k b
          else
            if c `elem` ['1'..'9']
              then
                k (toPosition (digitToInt c) --> b)
              else
                if c `elem` "qQ"
                  then
                    do line
                       putStrLn "Bye!"
                  else
                    do surround $ putStrLn "Invalid selection. Please try again."
                       gameLoop k b

tictactoe' ::
  Board
  -> IO ()
tictactoe' b =
  gameLoop (foldMoveResult
              (do surround $ putStrLn "That position is already taken. Try again."
                  printWithoutPositions b
                  line
                  tictactoe' b)
              (\b' -> do surround $ printWithoutPositions b'
                         tictactoe' b')
              (\b' -> do surround $ printWithoutPositions b'
                         putStrLn (playerGameResult "Player 1 Wins!" "Player 2 Wins!" "Draw" (getResult b'))))
            b

surround ::
  IO ()
  -> IO ()
surround a =
  do nlines 2
     a
     line

putStrLns ::
  [String]
  -> IO ()
putStrLns =
  mapM_ putStrLn

line ::
  IO ()
line =
  nlines 1

nlines ::
  Int
  -> IO ()
nlines n =
  replicateM_ n (putStrLn [])

printWithPositions ::
  BoardLike b =>
  b
  -> IO ()
printWithPositions =
  printPositions (show . fromPosition)

printWithoutPositions ::
  BoardLike b =>
  b
  -> IO ()
printWithoutPositions =
  printPositions (const " ")

printPositions ::
  BoardLike b =>
  (Position -> String)
  -> b
  -> IO ()
printPositions k b =
  printEachPosition (\p -> maybe (k p) (return . toSymbol) (b `playerAt` p))

fromPosition ::
  Position
  -> Int
fromPosition =
  succ . fromEnum

toPosition ::
  Int
  -> Position
toPosition n =
  toEnum (n - 1)

