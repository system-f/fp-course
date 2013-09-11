module Data.TicTacToe.ArbitraryPosition where

import Data.TicTacToe.Position
import Test.QuickCheck

instance Arbitrary Position where
  arbitrary =
    elements [minBound ..]
