module Test.Data.TicTacToe
(
  main
, tictactoeTests
) where

import Test.Data.TicTacToe.Board hiding (main)
import Test.Data.TicTacToe.Position hiding (main)
import Test.Data.TicTacToe.Player hiding (main)
import Test.Data.TicTacToe.GameResult hiding (main)
import Test.Framework

main ::
  IO ()
main =
  defaultMain tictactoeTests

tictactoeTests ::
  [Test]
tictactoeTests =
  [
    testGroup "TicTacToe" $
      concat
      [ boardTests
      , positionTests
      , playerTests
      , gameResultTests
      ]
  ]

