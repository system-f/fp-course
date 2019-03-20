{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.OptionalTest (
  -- * Tests
    test_Optional
  , mapOptionalTest
  , bindOptionalTest
  , valueOrTest
  , firstFullTest

  -- * Course test runner
  , courseTest
  ) where

import           Test.Course.Mini (courseTest)
import           Test.Mini        (MiniTestTree, testCase, testGroup, (@?=))

import           Course.Core
import           Course.Optional  (Optional (..), bindOptional, mapOptional,
                                   optional, (<+>), (??))

test_Optional :: MiniTestTree
test_Optional =
  testGroup "Optional" [
    mapOptionalTest
  , bindOptionalTest
  , valueOrTest
  , firstFullTest
  , optionalTest
  ]

mapOptionalTest :: MiniTestTree
mapOptionalTest =
  testGroup "mapOptional" [
    testCase "Empty" $
      mapOptional (+1) Empty @?= Empty
  , testCase "Full" $
      mapOptional (+1) (Full 8) @?= Full 9
  ]

bindOptionalTest :: MiniTestTree
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

valueOrTest :: MiniTestTree
valueOrTest =
  testGroup "??" [
    testCase "Full" $
      Full 8 ?? 99 @?= 8
  , testCase "Empty" $
      Empty ?? 99 @?= 99
  ]

firstFullTest :: MiniTestTree
firstFullTest =
  testGroup "<+>" [
    testCase "first Full" $
      Full 8 <+> Empty @?= Full 8
  , testCase "both Full" $
      Full 8 <+> Full 9 @?= Full 8
  , testCase "first Empty" $
      Empty <+> Full 9 @?= Full 9
  , testCase "both empty" $
      Empty <+> Empty @?= (Empty :: Optional Integer)
  ]

optionalTest :: MiniTestTree
optionalTest =
  testGroup "optional" [
    testCase "replaces full data constructor" $
      optional (+1) 0 (Full 8) @?= 9
  , testCase "replaces empty data constructor" $
      optional (+1) 0 Empty @?= 0
  ]
