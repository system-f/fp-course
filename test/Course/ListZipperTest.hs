{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipperTest where


import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import           Course.Core
import           Course.Functor        ((<$>))
import           Course.List           (List (..), isEmpty)
import           Course.ListZipper     (ListZipper, MaybeListZipper (..),
                                        dropLefts, dropRights, findLeft,
                                        findRight, fromList, hasLeft, hasRight,
                                        moveLeft, moveLeftLoop, moveRight,
                                        moveRightLoop, setFocus, swapLeft,
                                        swapRight, toList, toListZ, toOptional,
                                        withFocus, zipper, (-<<))
import           Course.Optional       (Optional (Empty))

import           Course.Gens           (forAllLists, forAllListsAndBool)

test_ListZipper :: TestTree
test_ListZipper =
  testGroup "ListZipper" [
    functorTest
  , functorMaybeTest
  , toListTest
  , fromListTest
  , toOptionalTest
  , withFocusTest
  , setFocusTest
  , hasLeftTest
  , hasRightTest
  , findLeftTest
  , findRightTest
  , moveLeftLoopTest
  , moveRightLoopTest
  , moveLeftTest
  , moveRightTest
  , swapLeftTest
  , swapRightTest
  , dropLeftsTest
  , dropRightsTest
  ]

functorTest :: TestTree
functorTest =
  testCase "ListZipper (<$>)" $
    (+1) <$> (zipper [3,2,1] 4 [5,6,7]) @?= zipper [4,3,2] 5 [6,7,8]

functorMaybeTest :: TestTree
functorMaybeTest =
  testCase "MaybeListZipper (<$>)" $
    (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7])) @?= IsZ (zipper [4,3,2] 5 [6,7,8])

toListTest :: TestTree
toListTest =
  testGroup "toList" [
    testCase "Optional empty list" $
      toList <$> Empty @?= (Empty :: Optional (List Int))
  , testCase "empty left" $
      toList (zipper [] 1 [2,3,4]) @?= (1:.2:.3:.4:.Nil)
  , testCase "lefts and rights" $
      toList (zipper [3,2,1] 4 [5,6,7]) @?= (1:.2:.3:.4:.5:.6:.7:.Nil)
  ]

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

hasLeftTest :: TestTree
hasLeftTest =
  testGroup "hasLeft" [
    testCase "left and right" $ hasLeft (zipper [1,0] 2 [3,4]) @?= True
  , testCase "empty left" $ hasLeft (zipper [] 0 [1,2]) @?= False
  ]

hasRightTest :: TestTree
hasRightTest =
  testGroup "hasRight" [
    testCase "left and right" $ hasRight (zipper [1,0] 2 [3,4]) @?= True
  , testCase "empty right" $ hasRight (zipper [1,0] 2 []) @?= False
  ]

findLeftTest :: TestTree
findLeftTest =
  testGroup "findLeft" [
    testProperty "missing element returns IsNotZ" $
      forAllListsAndBool (\(xs, p) -> findLeft (const p) -<< fromList xs == IsNotZ)
  , testCase "found in left" $
      findLeft (== 1) (zipper [2,1] 3 [4,5]) @?= IsZ (zipper [] 1 [2,3,4,5])
  , testCase "not found" $
      findLeft (== 6) (zipper [2,1] 3 [4,5]) @?= IsNotZ
  , testCase "one match in left" $
      findLeft (== 1) (zipper [2,1] 1 [4,5]) @?= IsZ (zipper [] 1 [2,1,4,5])
  , testCase "multiple matches in left" $
      findLeft (== 1) (zipper [1,2,1] 3 [4,5]) @?= IsZ (zipper [2,1] 1 [3,4,5])
  ]

findRightTest :: TestTree
findRightTest =
  testGroup "findRight" [
    testProperty "missing element returns IsNotZ" $
      forAllLists (\xs -> findRight (const False) -<< fromList xs == IsNotZ)
  , testCase "found in right" $
      findRight (== 5) (zipper [2,1] 3 [4,5]) @?= IsZ (zipper [4,3,2,1] 5 [])
  , testCase "not found" $
      findRight (== 6) (zipper [2,1] 3 [4,5]) @?= IsNotZ
  , testCase "one match in right" $
      findRight (== 1) (zipper [2,3] 1 [4,5,1]) @?= IsZ (zipper [5,4,1,2,3] 1 [])
  , testCase "multiple matches in right" $
      findRight (== 1) (zipper [2,3] 1 [1,4,5,1]) @?= IsZ (zipper [1,2,3] 1 [4,5,1])
  ]

moveLeftLoopTest :: TestTree
moveLeftLoopTest =
  testGroup "moveLeftLoop" [
    testCase "with left" $
      moveLeftLoop (zipper [3,2,1] 4 [5,6,7]) @?= zipper [2,1] 3 [4,5,6,7]
  , testCase "empty left" $
      moveLeftLoop (zipper [] 1 [2,3,4]) @?= zipper [3,2,1] 4 []
  ]

moveRightLoopTest :: TestTree
moveRightLoopTest =
  testGroup "moveRightLoop" [
    testCase "with right" $
      moveRightLoop (zipper [3,2,1] 4 [5,6,7]) @?= zipper [4,3,2,1] 5 [6,7]
  , testCase "empty right" $
      moveRightLoop (zipper [3,2,1] 4 []) @?= zipper [] 1 [2,3,4]
  ]

moveLeftTest :: TestTree
moveLeftTest =
  testGroup "moveLeft" [
    testCase "with left" $
      moveLeft (zipper [3,2,1] 4 [5,6,7]) @?= IsZ (zipper [2,1] 3 [4,5,6,7])
  , testCase "empty left" $
      moveLeft (zipper [] 1 [2,3,4]) @?= IsNotZ
  ]

moveRightTest :: TestTree
moveRightTest =
  testGroup "moveRight" [
    testCase "with right" $
      moveRight (zipper [3,2,1] 4 [5,6,7]) @?= IsZ (zipper [4,3,2,1] 5 [6,7])
  , testCase "empty right" $
      moveRight (zipper [3,2,1] 4 []) @?= IsNotZ
  ]

swapLeftTest :: TestTree
swapLeftTest =
  testGroup "swapLeft" [
    testCase "with left" $
      swapLeft (zipper [3,2,1] 4 [5,6,7]) @?= IsZ (zipper [4,2,1] 3 [5,6,7])
  , testCase "empty left" $
      swapLeft (zipper [] 1 [2,3,4]) @?= IsNotZ
  ]

swapRightTest :: TestTree
swapRightTest =
  testGroup "swapRight" [
    testCase "with right" $
      swapRight (zipper [3,2,1] 4 [5,6,7]) @?= IsZ (zipper [3,2,1] 5 [4,6,7])
  , testCase "empty right" $
      swapRight (zipper [3,2,1] 4 []) @?= IsNotZ
  ]

dropLeftsTest :: TestTree
dropLeftsTest =
  testGroup "dropLeft" [
    testCase "with left" $
      dropLefts (zipper [3,2,1] 4 [5,6,7]) @?= zipper [] 4 [5,6,7]
  , testCase "empty left" $
      dropLefts (zipper [] 1 [2,3,4]) @?= zipper [] 1 [2,3,4]
  , testProperty "dropLefts empties left of zipper"
      (\l x r -> dropLefts (zipper l x r) == (zipper [] x r :: ListZipper Integer))
  ]

dropRightsTest :: TestTree
dropRightsTest =
  testGroup "dropRights" [
    testCase "with right" $
      dropRights (zipper [3,2,1] 4 [5,6,7]) @?= zipper [3,2,1] 4 []
  , testCase "empty right" $
      dropRights (zipper [3,2,1] 4 []) @?= zipper [3,2,1] 4 []
  , testProperty "dropRights empties right of zipper"
      (\l x r -> dropRights (zipper l x r) == (zipper l x [] :: ListZipper Integer))
  ]
