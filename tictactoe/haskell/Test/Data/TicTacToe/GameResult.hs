module Test.Data.TicTacToe.GameResult
(
  main
, gameResultTests
) where

import Data.TicTacToe.GameResult
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Data.TicTacToe.Player()

instance Arbitrary GameResult where
  arbitrary = fmap (maybe draw win) arbitrary

main ::
  IO ()
main =
  defaultMain gameResultTests

gameResultTests ::
  [Test]
gameResultTests =
  [
    testGroup "GameResult"
      [
        testProperty "cata_ctor"  prop_cata_ctor
      , testProperty "cata_ctor2" prop_cata_ctor2
      , testProperty "is"         prop_is
      ]
  ]

prop_cata_ctor ::
  GameResult
  -> Bool
prop_cata_ctor r =
  gameResult win draw r == r

prop_cata_ctor2 ::
  GameResult
  -> Bool
prop_cata_ctor2 r =
  playerGameResult player1Wins player2Wins draw r == r

prop_is ::
  GameResult
  -> Bool
prop_is p =
  playerGameResult isPlayer1Wins isPlayer2Wins isDraw p p

