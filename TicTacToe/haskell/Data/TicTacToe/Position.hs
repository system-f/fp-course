-- | A position is one of the nine places on a tic-tac-toe grid.
module Data.TicTacToe.Position
(
  Position(..)
) where

-- A tic-tac-toe position.
data Position =
  NW -- ^ North-west (top left).
  | N -- ^ North (top centre).
  | NE -- ^ North-east (top right).
  | W -- ^ West (middle left).
  | C -- ^ Centre.
  | E -- ^ East (middle right)
  | SW -- ^ South-west (bottom left).
  | S -- ^ South (bottom centre).
  | SE -- ^ South-east (bottom right).
  deriving (Eq, Ord, Enum, Bounded)

instance Show Position where
  show NW = "NW"
  show N  = "N "
  show NE = "NE"
  show E  = "E "
  show SE = "SE"
  show S  = "S "
  show SW = "SW"
  show W  = "W "
  show C  = "C "
