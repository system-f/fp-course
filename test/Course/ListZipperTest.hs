{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipperTest where


import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import           Course.Core
import           Course.Functor        ((<$>))
import           Course.List           (List (..), isEmpty)
import           Course.ListZipper     (MaybeListZipper (..), fromList,
                                        setFocus, toList, toListZ, toOptional,
                                        withFocus, zipper)
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
  , withFocusTest
  , setFocusTest
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
  , testCase "empty left" $
      toList (zipper [] 1 [2,3,4]) @?= (1:.2:.3:.4:.Nil)
  , testCase "lefts and rights" $
      toList (zipper [3,2,1] 4 [5,6,7]) @?= (1:.2:.3:.4:.5:.6:.7:.Nil)
  ]

withFocusTest :: TestTree
withFocusTest =
  testGroup "withFocus" [
    testCase "empty left" $
      withFocus (+1) (zipper [] 0 [1]) @?= zipper [] 1 [1]
  , testCase "left and right" $
      withFocus (+1) (zipper [1,0] 2 [3,4]) @?= zipper [1,0] 3 [3,4]
  ]

setFocusTest :: TestTree
setFocusTest =
  testGroup "setFocus" [
    testCase "empty left" $
      setFocus 1 (zipper [] 0 [1]) @?= zipper [] 1 [1]
  , testCase "left and right" $
      setFocus 1 (zipper [1,0] 2 [3,4]) @?= zipper [1,0] 1 [3,4]
  ]
