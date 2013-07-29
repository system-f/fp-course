module Data.TicTacToe.ArbitraryGameResult where

import Data.TicTacToe.GameResult
import Data.TicTacToe.ArbitraryPlayer()
import Test.QuickCheck

instance Arbitrary GameResult where
  arbitrary = fmap (maybe draw win) arbitrary
