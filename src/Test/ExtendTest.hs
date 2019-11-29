{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ExtendTest (
  -- * Tests
    test_Extend
  , exactlyOneTest
  , listTest
  , optionalTest
  , cojoinTest

  -- * Runner
  , test
  ) where

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (ExactlyOne))
import           Course.Functor    ((<$>))
import           Course.List       (List ((:.), Nil), length, listh, reverse)
import           Course.Optional   (Optional (Empty, Full))

import           Course.Extend     (cojoin, (<<=))

import           Test.Framework  (TestTree, testCase, testGroup, test, (@?=))

test_Extend :: TestTree
test_Extend =
  testGroup "Extend" [
    exactlyOneTest
  , listTest
  , optionalTest
  , cojoinTest
  ]

exactlyOneTest :: TestTree
exactlyOneTest =
  testCase "ExactlyOne instance" $
    (id <<= ExactlyOne 7) @?= ExactlyOne (ExactlyOne 7)

listTest :: TestTree
listTest =
  testGroup "List" [
    testCase "length" $
      (length <<= ('a' :. 'b' :. 'c' :. Nil)) @?= (3 :. 2 :. 1 :. Nil)
  , testCase "id" $
      (id <<= (1 :. 2 :. 3 :. 4 :. Nil)) @?= nestedListh2 [[1,2,3,4],[2,3,4],[3,4],[4]]
  , testCase "reverse" $
      (reverse <<= ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. Nil)) @?=
        nestedListh3 [[[4,5,6],[1,2,3]],[[4,5,6]]]
  ]

optionalTest :: TestTree
optionalTest =
  testGroup "Optional" [
    testCase "id Full" $
      (id <<= Full 7) @?= Full (Full 7)
  , testCase "id Empty" $
      (id <<= Empty) @?= (Empty :: Optional (Optional Integer))
  ]

cojoinTest :: TestTree
cojoinTest =
  testGroup "cojoin" [
    testCase "ExactlyOne" $
      cojoin (ExactlyOne 7) @?= ExactlyOne (ExactlyOne 7)
  , testCase "List" $
      cojoin (1 :. 2 :. 3 :. 4 :. Nil) @?= nestedListh2 [[1,2,3,4],[2,3,4],[3,4],[4]]
  , testCase "Full" $
      cojoin (Full 7) @?= Full (Full 7)
  , testCase "Empty" $
      cojoin Empty @?= (Empty :: Optional (Optional Integer))
  ]

nestedListh2 :: [[a]] -> List (List a)
nestedListh2 = (listh <$>) . listh

nestedListh3 :: [[[a]]] -> List (List (List a))
nestedListh3 = ((listh <$>) <$>) . nestedListh2
