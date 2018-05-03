{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.OptionalTest where

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Course.Core
import           Course.Optional  (Optional (..), bindOptional, mapOptional,
                                   (<+>), (??))

test_Optional :: TestTree
test_Optional =
  testGroup "Optional" [
    mapOptionalTest
  , bindOptionalTest
  , valueOrTest
  , firstFullTest
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

valueOrTest :: TestTree
valueOrTest =
  testGroup "??" [
    testCase "Full" $
      Full 8 ?? 99 @?= 8
  , testCase "Empty" $
      Empty ?? 99 @?= 99
  ]

firstFullTest :: TestTree
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
