-- | A game result is one of
--
-- * Player 1 wins
--
-- * Player 2 wins
--
-- * Neither player wins (draw)
module Data.TicTacToe.GameResult
(
  GameResult
-- * Reduction (fold)
, gameResult
, playerGameResult
-- * Construction
, win
, player1Wins
, player2Wins
, draw
-- * Decisions
, isPlayer1Wins
, isPlayer2Wins
, isDraw
) where

import Data.TicTacToe.Player

-- | A game result.
data GameResult =
  Win Player
  | Draw
  deriving (Eq, Show)

-- | Fold a game result.
gameResult ::
  (Player -> x) -- ^ If either of the players won.
  -> x -- ^ If the game was a draw.
  -> GameResult -- ^ The game result to fold.
  -> x
gameResult win _    (Win p) =
  win p
gameResult _   draw Draw    =
  draw

-- | Fold a game result.
playerGameResult ::
  x -- ^ If player 1 won.
  -> x -- ^ If player 2 won.
  -> x -- ^ If the game was a draw.
  -> GameResult -- ^ The game result to fold.
  -> x
playerGameResult p1 p2 =
  gameResult (player p1 p2)

-- | Construct a game result with a win for the given player.
win ::
  Player -- ^ The player to win.
  -> GameResult
win =
  Win

-- | Construct a game result with a win for player 1.
player1Wins ::
  GameResult
player1Wins =
  Win player1

-- | Construct a game result with a win for player 2.
player2Wins ::
  GameResult
player2Wins =
  Win player2

-- | Construct a game result that is a draw.
draw ::
  GameResult
draw =
  Draw

-- | Returns whether or not player 1 won for the game result.
isPlayer1Wins ::
  GameResult
  -> Bool
isPlayer1Wins =
  playerGameResult True False False

-- | Returns whether or not player 2 won for the game result.
isPlayer2Wins ::
  GameResult
  -> Bool
isPlayer2Wins =
  playerGameResult False True False

-- | Returns whether the game result is a draw.
isDraw ::
  GameResult
  -> Bool
isDraw =
  playerGameResult False False True
