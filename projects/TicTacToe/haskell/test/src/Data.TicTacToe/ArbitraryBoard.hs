module Data.TicTacToe.ArbitraryBoard where

import Data.TicTacToe.Board
import Data.TicTacToe.ArbitraryPosition()
import Test.QuickCheck

instance Arbitrary Board where
  arbitrary =
    do p <- arbitrary
       q <- resize 12 arbitrary
       return $ Prelude.foldr (\p b -> keepPlayingOr b id (p --> b)) (p --> empty) q
