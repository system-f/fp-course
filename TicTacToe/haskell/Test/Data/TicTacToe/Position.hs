module Test.Data.TicTacToe.Position
(
  main
, positionTests
) where

import Data.TicTacToe.Position
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary Position where
  arbitrary = elements [minBound ..]

main ::
  IO ()
main =
  defaultMain positionTests

positionTests ::
  [Test]
positionTests =
  [
    testGroup "Position"
      [
        testProperty "show is length 2" prop_show_is_two
      ]
  ]

prop_show_is_two ::
  Position
  -> Bool
prop_show_is_two p =
  length (show p) == 2

