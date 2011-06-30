module Test.Data.TicTacToe.Player
(
  main
, playerTests
) where

import Data.TicTacToe.Player
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary Player where
  arbitrary = elements [player1, player2]


main ::
  IO ()
main =
  defaultMain playerTests

playerTests ::
  [Test]
playerTests =
  [
    testGroup "Player"
      [
        testProperty "is_exclusive" prop_is_exclusive
      , testProperty "cata_ctor"    prop_cata_ctor
      , testProperty "alternate"    prop_alternate
      ]
  ]

prop_is_exclusive ::
  Player
  -> Bool
prop_is_exclusive p =
  isPlayer1 p /= isPlayer2 p

prop_cata_ctor ::
  Player
  -> Bool
prop_cata_ctor p =
  player player1 player2 p == p

prop_alternate ::
  Player
  -> Bool
prop_alternate p =
  (alternate p /= p) && (alternate (alternate p) == p)

