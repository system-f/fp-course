module Main where

import qualified L01.Validation.Tests
import qualified L02.List.Tests
import qualified L03.Misty.Tests
import qualified L03.State.Tests
import qualified L04.ListZipper.Tests
import qualified L05.Parser.Tests
import Test.Framework

main ::
  IO ()
main =
  defaultMain tests

tests ::
  [Test]
tests =
  [
    testGroup "Tests"
      [
        L01.Validation.Tests.test
      , L02.List.Tests.test
      , L03.Misty.Tests.test
      , L03.State.Tests.test
      , L04.ListZipper.Tests.test
      , L05.Parser.Tests.test
      ]
  ]

