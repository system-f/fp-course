{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipperTest where


import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import           Course.Core
import           Course.Functor        ((<$>))
import           Course.List           (List (..), isEmpty)
import           Course.ListZipper     (MaybeListZipper (..), fromList, toListZ,
                                        toOptional, zipper, toList)
import           Course.Optional       (Optional (Empty))

import           Course.ListTest       (forAllLists)

test_ListZipper :: TestTree
test_ListZipper =
  testGroup "ListZipper" [
    functorTest
  , functorMaybeTest
  , fromListTest
  , toOptionalTest
  , toListTest
  ]

functorTest :: TestTree
functorTest =
  testCase "ListZipper (<$>)" $
    (+1) <$> (zipper [3,2,1] 4 [5,6,7]) @?= zipper [4,3,2] 5 [6,7,8]

functorMaybeTest :: TestTree
functorMaybeTest =
  testCase "MaybeListZipper (<$>)" $
    (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7])) @?= IsZ (zipper [4,3,2] 5 [6,7,8])

fromListTest :: TestTree
fromListTest =
  testGroup "fromList" [
    testCase "non-empty" $ fromList (1 :. 2 :. 3 :. Nil) @?= IsZ (zipper [] 1 [2,3])
  , testCase "empty" $ fromList Nil @?= (IsNotZ :: MaybeListZipper Integer)
  , testProperty "round trip" $
      forAllLists (\xs -> toListZ (fromList xs) == xs)
  ]

toOptionalTest :: TestTree
toOptionalTest =
  testGroup "toOptional" [
    testProperty "empty" $
      forAllLists (\xs -> isEmpty xs == (toOptional (fromList xs) == Empty))
  ]

toListTest :: TestTree
toListTest =
  testGroup "toList" [
    testCase "Optional empty list" $
      toList <$> toOptional (fromList Nil) @?= (Empty :: Optional (List Int))
  , testCase "empty lefts" $
      toList (zipper [] 1 [2,3,4]) @?= (1:.2:.3:.4:.Nil)
  , testCase "lefts and rights" $
      toList (zipper [3,2,1] 4 [5,6,7]) @?= (1:.2:.3:.4:.5:.6:.7:.Nil)
  ]
