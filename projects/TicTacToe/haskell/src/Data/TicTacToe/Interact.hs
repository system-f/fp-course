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
import System.IO

-- | Play tic-tac-toe interactively.
tictactoe ::
  IO ()
tictactoe =
  do hSetBuffering stdin NoBuffering
     putStrLn . showWithPositions $ empty
     gameLoop (\b' -> do surround . putStrLn . showWithoutPositions $ b'
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
            show p ++ " to move [" ++ toSymbol p : "]"
          , "  [1-9] to Move"
          , "  q to Quit"
          , "  v to view positions"
          ]
        c <- getChar
        if c `elem` "vV"
          then
            do surround . putStrLn . showWithPositions $ b
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
                    do surround . putStrLn $ ("Invalid selection '" ++ c : "'. Please try again.")
                       gameLoop k b

tictactoe' ::
  Board
  -> IO ()
tictactoe' b =
  gameLoop (foldMoveResult
              (do surround $ putStrLn "That position is already taken. Try again."
                  putStrLn . showWithoutPositions $ b
                  line
                  tictactoe' b)
              (\b' -> do surround . putStrLn . showWithoutPositions $ b'
                         tictactoe'  b')
              (\b' -> do surround . putStrLn . showWithoutPositions $ b'
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

showWithPositions ::
  BoardLike b =>
  b
  -> String
showWithPositions b =
  showEachPosition (\p -> maybe (show . fromPosition $ p) (return . toSymbol) (b `playerAt` p))

showWithoutPositions ::
  BoardLike b =>
  b
  -> String
showWithoutPositions b =
  showEachPosition (\p -> maybe " " (return . toSymbol) (b `playerAt` p))

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
