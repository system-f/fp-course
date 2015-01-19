module Data.TicTacToe.ArbitraryPlayer where

import Data.TicTacToe.Player
import Test.QuickCheck

instance Arbitrary Player where
  arbitrary =
    elements [player1, player2]
