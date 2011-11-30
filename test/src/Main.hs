module Main where

import qualified L01.Validation.Tests
import qualified L02.List.Tests
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
      ]
  ]

