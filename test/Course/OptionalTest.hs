{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Course.OptionalTest where

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import Course.Core
import Course.Optional (Optional (..), mapOptional, bindOptional)

test_Optional :: TestTree
test_Optional =
  testGroup "Optional" [
    mapOptionalTest
  , bindOptionalTest
  ]

mapOptionalTest :: TestTree
mapOptionalTest =
  testGroup "mapOptional" [
    testCase "Empty" $
      mapOptional (+1) Empty @?= Empty
  , testCase "Full" $
      mapOptional (+1) (Full 8) @?= Full 9
  ]

bindOptionalTest :: TestTree
bindOptionalTest =
  let evenDecOddInc n = if even n then Full (n - 1) else Full (n + 1)
   in testGroup "bindOptional" [
        testCase "Empty" $
          bindOptional Full Empty @?= (Empty :: Optional Integer)
      , testCase "even dec, odd inc, even input" $
          bindOptional evenDecOddInc (Full 8) @?= Full 7
      , testCase "even dec, odd inc, odd input" $
          bindOptional evenDecOddInc (Full 9) @?= Full 10
  ]
