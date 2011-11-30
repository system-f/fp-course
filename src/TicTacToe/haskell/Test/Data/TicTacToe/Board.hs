module Test.Data.TicTacToe.Board
(
  main
, boardTests
) where


import Prelude hiding (all, any)
import Data.TicTacToe.Board
import Data.TicTacToe.Position
import Data.Foldable
import Test.Data.TicTacToe.Position()
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance Arbitrary Board where
  arbitrary =
    do p <- arbitrary
       q <- resize 12 arbitrary
       return $ Prelude.foldr (\p b -> keepPlayingOr b id (p --> b)) (p --> empty) q


main ::
  IO ()
main =
  defaultMain boardTests

boardTests ::
  [Test]
boardTests =
  [
    testGroup "Board"
      [
        testProperty "whoseTurn"       prop_whoseTurn
      , testProperty "move_whoseTurn"  prop_move_whoseTurn
      , testProperty "move_moveBack"   prop_move_takeBack
      ]
  ]

prop_whoseTurn ::
  Board
  -> Bool
prop_whoseTurn b =
  whoseTurn b /= whoseNotTurn b

prop_move_whoseTurn ::
  Board
  -> Position
  -> Bool
prop_move_whoseTurn b p =
  (\b' -> whoseTurn b /= whoseTurn b') `all` keepPlaying (p --> b)

prop_move_takeBack ::
  Board
  -> Position
  -> Bool
prop_move_takeBack b p =
  foldMoveResult True (foldTakenBack False (==b) . takeBack) (\fb -> takeBack fb == b) (p --> b)
