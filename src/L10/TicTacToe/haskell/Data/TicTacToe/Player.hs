-- | A player is either player 1 or player 2 /(isomorphic to Bool)/.
module Data.TicTacToe.Player
(
Player
-- * Reduction
, player
-- * Construction
, player1
, player2
-- * Decisions
, isPlayer1
, isPlayer2
-- * Combinator
, alternate
, toSymbol
) where

-- | A player.
data Player =
  Player1
  | Player2
  deriving (Eq, Ord, Enum)

instance Show Player where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

-- | Returns whether or not the player is player 1.
isPlayer1 ::
  Player ->
  Bool
isPlayer1 Player1 =
  True
isPlayer1 Player2 =
  False

-- | Returns whether or not the player is player 2.
isPlayer2 ::
  Player
  -> Bool
isPlayer2 =
  not . isPlayer1

-- | Player 1.
player1 ::
  Player
player1 =
  Player1

-- | Player 2.
player2 ::
  Player
player2 =
  Player2

-- | Folds a player.
player ::
  x -- ^ If player 1.
  -> x -- ^ If player 2.
  -> Player -- ^ The player to fold.
  -> x
player x _ Player1 =
  x
player _ x Player2 =
  x

-- | Switches a player from player 1 to player 2 or vice versa.
alternate ::
  Player -- ^ The player to alternate.
  -> Player
alternate Player1 =
  Player2
alternate Player2 =
  Player1

-- | Returns a character symbol denoting each player.
toSymbol ::
  Player
  -> Char
toSymbol =
  player 'X' 'O'
