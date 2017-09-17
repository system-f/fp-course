{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipperTest where


import           Prelude               (fromIntegral)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import           Course.Core
import           Course.Functor        ((<$>))
import           Course.List           (List (..), isEmpty)
import           Course.ListZipper     (ListZipper, MaybeListZipper (..),
                                        deletePullLeft, deletePullRight,
                                        dropLefts, dropRights, end, findLeft,
                                        findRight, fromList, hasLeft, hasRight,
                                        index, insertPushLeft, insertPushRight,
                                        lefts, moveLeft, moveLeftLoop,
                                        moveLeftN, moveLeftN', moveRight,
                                        moveRightLoop, moveRightN, moveRightN',
                                        nth, rights, setFocus, start, swapLeft,
                                        swapRight, toList, toListZ, toOptional,
                                        withFocus, zipper, (-<<))
import           Course.Optional       (Optional (Empty, Full))

import           Course.Gens           (forAllListZipper,
                                        forAllListZipperWithInt, forAllLists,
                                        forAllListsAndBool)

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
  , moveLeftNTest
  , moveRightNTest
  , moveLeftN'Test
  , moveRightN'Test
  , nthTest
  , indexTest
  , endTest
  , startTest
  , deletePullLeftTest
  , deletePullRightTest
  , insertPushLeftTest
  , insertPushRightTest
  , applicativeTest
  , applicativeMaybeTest
  , extendTest
  , extendMaybeTest
  , comonadTest
  , traversableTest
  , traversableMaybeTest
  ]

functorTest :: TestTree
functorTest =
  testCase "ListZipper (<$>)" $
    (+1) <$> defaultZipper @?= zipper [4,3,2] 5 [6,7,8]

functorMaybeTest :: TestTree
functorMaybeTest =
  testCase "MaybeListZipper (<$>)" $
    (+1) <$> (IsZ defaultZipper) @?= IsZ (zipper [4,3,2] 5 [6,7,8])

toListTest :: TestTree
toListTest =
  testGroup "toList" [
    testCase "Optional empty list" $
      toList <$> Empty @?= (Empty :: Optional (List Int))
  , testCase "empty left" $
      toList (zipper [] 1 [2,3,4]) @?= (1:.2:.3:.4:.Nil)
  , testCase "lefts and rights" $
      toList defaultZipper @?= (1:.2:.3:.4:.5:.6:.7:.Nil)
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
  , testCase "elements shifted to right correctly" $
      findLeft (== 1) (zipper [3,4,1,5] 9 [2,7]) @?= IsZ (zipper [5] 1 [4,3,9,2,7])
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
      moveLeftLoop defaultZipper @?= zipper [2,1] 3 [4,5,6,7]
  , testCase "empty left" $
      moveLeftLoop (zipper [] 1 [2,3,4]) @?= zipper [3,2,1] 4 []
  ]

moveRightLoopTest :: TestTree
moveRightLoopTest =
  testGroup "moveRightLoop" [
    testCase "with right" $
      moveRightLoop defaultZipper @?= zipper [4,3,2,1] 5 [6,7]
  , testCase "empty right" $
      moveRightLoop (zipper [3,2,1] 4 []) @?= zipper [] 1 [2,3,4]
  ]

moveLeftTest :: TestTree
moveLeftTest =
  testGroup "moveLeft" [
    testCase "with left" $
      moveLeft defaultZipper @?= IsZ (zipper [2,1] 3 [4,5,6,7])
  , testCase "empty left" $
      moveLeft (zipper [] 1 [2,3,4]) @?= IsNotZ
  ]

moveRightTest :: TestTree
moveRightTest =
  testGroup "moveRight" [
    testCase "with right" $
      moveRight defaultZipper @?= IsZ (zipper [4,3,2,1] 5 [6,7])
  , testCase "empty right" $
      moveRight (zipper [3,2,1] 4 []) @?= IsNotZ
  ]

swapLeftTest :: TestTree
swapLeftTest =
  testGroup "swapLeft" [
    testCase "with left" $
      swapLeft defaultZipper @?= IsZ (zipper [4,2,1] 3 [5,6,7])
  , testCase "empty left" $
      swapLeft (zipper [] 1 [2,3,4]) @?= IsNotZ
  ]

swapRightTest :: TestTree
swapRightTest =
  testGroup "swapRight" [
    testCase "with right" $
      swapRight defaultZipper @?= IsZ (zipper [3,2,1] 5 [4,6,7])
  , testCase "empty right" $
      swapRight (zipper [3,2,1] 4 []) @?= IsNotZ
  ]

dropLeftsTest :: TestTree
dropLeftsTest =
  testGroup "dropLeft" [
    testCase "with left" $
      dropLefts defaultZipper @?= zipper [] 4 [5,6,7]
  , testCase "empty left" $
      dropLefts (zipper [] 1 [2,3,4]) @?= zipper [] 1 [2,3,4]
  , testProperty "dropLefts empties left of zipper"
      (\l x r -> dropLefts (zipper l x r) == (zipper [] x r :: ListZipper Integer))
  ]

dropRightsTest :: TestTree
dropRightsTest =
  testGroup "dropRights" [
    testCase "with right" $
      dropRights defaultZipper @?= zipper [3,2,1] 4 []
  , testCase "empty right" $
      dropRights (zipper [3,2,1] 4 []) @?= zipper [3,2,1] 4 []
  , testProperty "dropRights empties right of zipper"
      (\l x r -> dropRights (zipper l x r) == (zipper l x [] :: ListZipper Integer))
  ]

moveLeftNTest :: TestTree
moveLeftNTest =
  testGroup "moveLeftN" [
    testCase "positive moves" $
      moveLeftN 2 (zipper [2,1,0] 3 [4,5,6]) @?= IsZ (zipper [0] 1 [2,3,4,5,6])
  , testCase "negative moves" $
      moveLeftN (-1) (zipper [2,1,0] 3 [4,5,6]) @?= IsZ (zipper [3,2,1,0] 4 [5,6])
  ]

moveRightNTest :: TestTree
moveRightNTest =
  testGroup "moveRightN" [
    testCase "positive moves" $
      moveRightN 1 (zipper [2,1,0] 3 [4,5,6]) @?= IsZ (zipper [3,2,1,0] 4 [5,6])
  , testCase "negative moves" $
      moveRightN (-1) (zipper [2,1,0] 3 [4,5,6]) @?= IsZ (zipper [1,0] 2 [3,4,5,6])
  ]

moveLeftN'Test :: TestTree
moveLeftN'Test =
  testGroup "moveLeftN'" [
    testCase "positive - out of bounds both sides" $
      moveLeftN' 4 defaultZipper @?= Left 3
  , testCase "positive in range" $
      moveLeftN' 1 defaultZipper @?= Right (zipper [2,1] 3 [4,5,6,7])
  , testProperty "moving zero is `Right . id`" $
      (\l x r -> let lz = (zipper l x r :: ListZipper Integer) in moveLeftN' 0 lz == (Right . id $ lz))
  , testCase "negative in range" $
      moveLeftN' (-2) defaultZipper @?= Right (zipper [5,4,3,2,1] 6 [7])
  , testCase "negative out of bounds" $
      moveLeftN' (-4 ) defaultZipper @?= Left 3
  , testCase "positive - out of bounds on left only" $
      moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9]) @?= Left 3
  , testCase "negative - out of bounds on right only" $
      moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9]) @?= Left 3
  ]

moveRightN'Test :: TestTree
moveRightN'Test =
  testGroup "moveRightN'" [
    testCase "positive - out of bounds both sides" $
      moveRightN' 4 defaultZipper @?= Left 3
  , testCase "positive in range" $
      moveRightN' 1 defaultZipper @?= Right (zipper [4,3,2,1] 5 [6,7])
  , testProperty "moving zero is `Right . id`" $
      (\l x r -> let lz = (zipper l x r :: ListZipper Integer) in moveRightN' 0 lz == (Right . id $ lz))
  , testCase "negative in range" $
      moveRightN' (-2) defaultZipper @?= Right (zipper [1] 2 [3,4,5,6,7])
  , testCase "negative - out of bounds both sides" $
      moveRightN' (-4) defaultZipper @?= Left 3
  ]

nthTest :: TestTree
nthTest =
  testGroup "nth" [
    testCase "have 1"    $ nth 1 defaultZipper @?= IsZ (zipper [1] 2 [3,4,5,6,7])
  , testCase "have 5"    $ nth 5 defaultZipper @?= IsZ (zipper [5,4,3,2,1] 6 [7])
  , testCase "missing 8" $ nth 8 defaultZipper @?= IsNotZ
  ]

indexTest :: TestTree
indexTest =
  testGroup "index" [
    testCase "index works" $ index defaultZipper @?= 3
  , testProperty "Always returns the index on a valid zipper" $
      forAllListZipperWithInt (\(z,i) -> optional True (\z' -> index z' == i) (toOptional (nth i z)))
  ]

endTest :: TestTree
endTest =
  testGroup "end" [
    testCase "end" $ end defaultZipper @?= zipper [6,5,4,3,2,1] 7 []
  , testProperty "end never changes the zipper's contents" $
      forAllListZipper (\z -> toList z == toList (end z))
  , testProperty "never have rights after calling end" $
      forAllListZipper (\z -> rights (end z) == Nil)
  ]

startTest :: TestTree
startTest =
  testGroup "start" [
    testCase "start" $ start defaultZipper @?= zipper [] 1 [2,3,4,5,6,7]
  , testProperty "start never changes the zipper's contents" $
      forAllListZipper (\z -> toList z == toList (start z))
  , testProperty "never have lefts after calling start" $
      forAllListZipper (\z -> lefts (start z) == Nil)
  ]

deletePullLeftTest :: TestTree
deletePullLeftTest =
  testGroup "deletePullLeft" [
    testCase "non-empty lefts" $ deletePullLeft defaultZipper @?= IsZ (zipper [2,1] 3 [5,6,7])
  , testCase "empty lefts" $ deletePullLeft (zipper [] 1 [2,3,4]) @?= IsNotZ
  ]

deletePullRightTest :: TestTree
deletePullRightTest =
  testGroup "deletePullRight" [
    testCase "non-empty rights" $ deletePullRight defaultZipper @?= IsZ (zipper [3,2,1] 5 [6,7])
  , testCase "empty rights" $ deletePullLeft (zipper [3,2,1] 4 []) @?= IsNotZ
  ]

insertPushLeftTest :: TestTree
insertPushLeftTest =
  testGroup "insertPushLeft" [
    testCase "non-empty lefts" $
      insertPushLeft 15 defaultZipper @?= zipper [4,3,2,1] 15 [5,6,7]
  , testCase "empty lefts" $
      insertPushLeft 15 (zipper [] 1 [2,3,4]) @?= zipper [1] 15 [2,3,4]
  , testProperty "deletePullLeft . insertPushLeft == id" $
      forAllListZipperWithInt (\(z,i) -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft (fromIntegral i) z))))
  ]

insertPushRightTest :: TestTree
insertPushRightTest =
  testGroup "insertPushRight" [
    testCase "non-empty rights" $
      insertPushRight 15 defaultZipper @?= zipper [3,2,1] 15 [4,5,6,7]
  , testCase "empty rights" $
      insertPushRight 15 (zipper [3,2,1] 4 []) @?= zipper [3,2,1] 15 [4]
  , testProperty "deletePullRight . insertPushRight == id" $
      forAllListZipperWithInt (\(z,i) -> optional False (==z) (toOptional (deletePullRight (insertPushRight (fromIntegral i) z))))
  ]

applicativeTest :: TestTree
applicativeTest = error "todo"

applicativeMaybeTest :: TestTree
applicativeMaybeTest = error "todo"

extendTest :: TestTree
extendTest = error "todo"

extendMaybeTest :: TestTree
extendMaybeTest = error "todo"

comonadTest :: TestTree
comonadTest = error "todo"

traversableTest :: TestTree
traversableTest = error "todo"

traversableMaybeTest :: TestTree
traversableMaybeTest = error "todo"

defaultZipper :: ListZipper Integer
defaultZipper = zipper [3,2,1] 4 [5,6,7]

optional :: b -> (a -> b) -> Optional a -> b
optional e _ Empty    = e
optional _ f (Full a) = f a
