{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Test.ListZipperTest (
  -- * Tests
    test_ListZipper
  , functorTest
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

  -- * Runner
  , test
  ) where


import qualified Prelude            as P (fromIntegral, (<$>))

import           Test.Framework     (TestTree, testCase, testGroup,
                                     testProperty, test, (@?=))
import           Test.Framework.Property (getPositive, getUnshowable)

import           Course.Applicative (pure, (<*>))
import           Course.Comonad     (copure)
import           Course.Core
import           Course.Extend      ((<<=))
import           Course.Functor     ((<$>))
import           Course.List        (List ((:.), Nil), all, isEmpty, take)
import           Course.ListZipper  (ListZipper (ListZipper),
                                     MaybeListZipper (..), deletePullLeft,
                                     deletePullRight, dropLefts, dropRights,
                                     end, findLeft, findRight, fromList,
                                     hasLeft, hasRight, index, insertPushLeft,
                                     insertPushRight, isZ, isNotZ, lefts, moveLeft,
                                     moveLeftLoop, moveLeftN, moveLeftN',
                                     moveRight, moveRightLoop, moveRightN,
                                     moveRightN', nth, rights, setFocus, start,
                                     swapLeft, swapRight, toList, toListZ,
                                     toOptional, withFocus, zipper, (-<<))
import           Course.Optional    (Optional (Empty, Full))
import           Course.Traversable (traverse)

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
    (+1) <$> zipper [3,2,1] 4 [5,6,7] @?= zipper [4,3,2] 5 [6,7,8]

functorMaybeTest :: TestTree
functorMaybeTest =
  testCase "MaybeListZipper (<$>)" $
    (+1) <$> isZ (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [4,3,2] 5 [6,7,8])

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
    testCase "non-empty" $ fromList (1 :. 2 :. 3 :. Nil) @?= isZ (zipper [] 1 [2,3])
  , testCase "empty" $ fromList Nil @?= (isNotZ :: MaybeListZipper Integer)
  , testProperty "round trip" $ \xs ->
      toListZ (fromList xs) == (xs :: List Integer)
  ]

toOptionalTest :: TestTree
toOptionalTest =
  testGroup "toOptional" [
    testProperty "empty" $ \xs ->
      isEmpty xs == (toOptional (fromList xs) == (Empty :: Optional (ListZipper Integer)))
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
    testProperty "missing element returns isNotZ" $ \xs p ->
      findLeft (const p) -<< fromList xs == (isNotZ :: MaybeListZipper Integer)
  , testCase "found in left" $
      findLeft (== 1) (zipper [2,1] 3 [4,5]) @?= isZ (zipper [] 1 [2,3,4,5])
  , testCase "not found" $
      findLeft (== 6) (zipper [2,1] 3 [4,5]) @?= isNotZ
  , testCase "one match in left" $
      findLeft (== 1) (zipper [2,1] 1 [4,5]) @?= isZ (zipper [] 1 [2,1,4,5])
  , testCase "multiple matches in left" $
      findLeft (== 1) (zipper [1,2,1] 3 [4,5]) @?= isZ (zipper [2,1] 1 [3,4,5])
  , testCase "elements shifted to right correctly" $
      findLeft (== 1) (zipper [3,4,1,5] 9 [2,7]) @?= isZ (zipper [5] 1 [4,3,9,2,7])
  ]

findRightTest :: TestTree
findRightTest =
  testGroup "findRight" [
    testProperty "missing element returns isNotZ" $ \xs ->
      findRight (const False) -<< fromList xs == (isNotZ :: MaybeListZipper Integer)
  , testCase "found in right" $
      findRight (== 5) (zipper [2,1] 3 [4,5]) @?= isZ (zipper [4,3,2,1] 5 [])
  , testCase "not found" $
      findRight (== 6) (zipper [2,1] 3 [4,5]) @?= isNotZ
  , testCase "one match in right" $
      findRight (== 1) (zipper [2,3] 1 [4,5,1]) @?= isZ (zipper [5,4,1,2,3] 1 [])
  , testCase "multiple matches in right" $
      findRight (== 1) (zipper [2,3] 1 [1,4,5,1]) @?= isZ (zipper [1,2,3] 1 [4,5,1])
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
      moveLeft (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [2,1] 3 [4,5,6,7])
  , testCase "empty left" $
      moveLeft (zipper [] 1 [2,3,4]) @?= isNotZ
  ]

moveRightTest :: TestTree
moveRightTest =
  testGroup "moveRight" [
    testCase "with right" $
      moveRight (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [4,3,2,1] 5 [6,7])
  , testCase "empty right" $
      moveRight (zipper [3,2,1] 4 []) @?= isNotZ
  ]

swapLeftTest :: TestTree
swapLeftTest =
  testGroup "swapLeft" [
    testCase "with left" $
      swapLeft (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [4,2,1] 3 [5,6,7])
  , testCase "empty left" $
      swapLeft (zipper [] 1 [2,3,4]) @?= isNotZ
  ]

swapRightTest :: TestTree
swapRightTest =
  testGroup "swapRight" [
    testCase "with right" $
      swapRight (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [3,2,1] 5 [4,6,7])
  , testCase "empty right" $
      swapRight (zipper [3,2,1] 4 []) @?= isNotZ
  ]

dropLeftsTest :: TestTree
dropLeftsTest =
  testGroup "dropLeft" [
    testCase "with left" $
      dropLefts (zipper [3,2,1] 4 [5,6,7]) @?= zipper [] 4 [5,6,7]
  , testCase "empty left" $
      dropLefts (zipper [] 1 [2,3,4]) @?= zipper [] 1 [2,3,4]
  , testProperty "dropLefts empties left of zipper" $ \lz@(ListZipper _ x r) ->
      dropLefts lz == (ListZipper Nil x r :: ListZipper Integer)
  ]

dropRightsTest :: TestTree
dropRightsTest =
  testGroup "dropRights" [
    testCase "with right" $
      dropRights (zipper [3,2,1] 4 [5,6,7]) @?= zipper [3,2,1] 4 []
  , testCase "empty right" $
      dropRights (zipper [3,2,1] 4 []) @?= zipper [3,2,1] 4 []
  , testProperty "dropRights empties right of zipper" $ \lz@(ListZipper l x _) ->
      dropRights lz == (ListZipper l x Nil :: ListZipper Integer)
  ]

moveLeftNTest :: TestTree
moveLeftNTest =
  testGroup "moveLeftN" [
    testCase "positive moves" $
      moveLeftN 2 (zipper [2,1,0] 3 [4,5,6]) @?= isZ (zipper [0] 1 [2,3,4,5,6])
  , testCase "negative moves" $
      moveLeftN (-1) (zipper [2,1,0] 3 [4,5,6]) @?= isZ (zipper [3,2,1,0] 4 [5,6])
  ]

moveRightNTest :: TestTree
moveRightNTest =
  testGroup "moveRightN" [
    testCase "positive moves" $
      moveRightN 1 (zipper [2,1,0] 3 [4,5,6]) @?= isZ (zipper [3,2,1,0] 4 [5,6])
  , testCase "negative moves" $
      moveRightN (-1) (zipper [2,1,0] 3 [4,5,6]) @?= isZ (zipper [1,0] 2 [3,4,5,6])
  ]

moveLeftN'Test :: TestTree
moveLeftN'Test =
  testGroup "moveLeftN'" [
    testCase "positive - out of bounds both sides" $
      moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7]) @?= Left 3
  , testCase "positive in range" $
      moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7]) @?= Right (zipper [2,1] 3 [4,5,6,7])
  , testProperty "moving zero is `Right . id`" $ \lz ->
      moveLeftN' 0 lz == (Right . id $ lz :: Either Int (ListZipper Integer))
  , testCase "negative in range" $
      moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7]) @?= Right (zipper [5,4,3,2,1] 6 [7])
  , testCase "negative out of bounds" $
      moveLeftN' (-4 ) (zipper [3,2,1] 4 [5,6,7]) @?= Left 3
  , testCase "positive - out of bounds on left only" $
      moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9]) @?= Left 3
  , testCase "negative - out of bounds on right only" $
      moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9]) @?= Left 3
  ]

moveRightN'Test :: TestTree
moveRightN'Test =
  testGroup "moveRightN'" [
    testCase "positive - out of bounds both sides" $
      moveRightN' 4 (zipper [3,2,1] 4 [5,6,7]) @?= Left 3
  , testCase "positive in range" $
      moveRightN' 1 (zipper [3,2,1] 4 [5,6,7]) @?= Right (zipper [4,3,2,1] 5 [6,7])
  , testProperty "moving zero is `Right . id`" $ \lz ->
      moveRightN' 0 lz == (Right . id $ lz :: Either Int (ListZipper Integer))
  , testCase "negative in range" $
      moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7]) @?= Right (zipper [1] 2 [3,4,5,6,7])
  , testCase "negative - out of bounds both sides" $
      moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7]) @?= Left 3
  ]

nthTest :: TestTree
nthTest =
  testGroup "nth" [
    testCase "have 1"    $ nth 1 (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [1] 2 [3,4,5,6,7])
  , testCase "have 5"    $ nth 5 (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [5,4,3,2,1] 6 [7])
  , testCase "missing 8" $ nth 8 (zipper [3,2,1] 4 [5,6,7]) @?= isNotZ
  ]

indexTest :: TestTree
indexTest =
  testGroup "index" [
    testCase "index works" $ index (zipper [3,2,1] 4 [5,6,7]) @?= 3
  , testProperty "Always returns the index on a valid zipper" $ \z i ->
      optional True (\z' -> index (z' :: ListZipper Integer) == i) (toOptional (nth i z))
  ]

endTest :: TestTree
endTest =
  testGroup "end" [
    testCase "end" $ end (zipper [3,2,1] 4 [5,6,7]) @?= zipper [6,5,4,3,2,1] 7 []
  , testProperty "end never changes the zipper's contents" $ \z ->
      toList z == (toList (end z) :: List Integer)
  , testProperty "never have rights after calling end" $ \z ->
      rights (end z) == (Nil :: List Integer)
  ]

startTest :: TestTree
startTest =
  testGroup "start" [
    testCase "start" $ start (zipper [3,2,1] 4 [5,6,7]) @?= zipper [] 1 [2,3,4,5,6,7]
  , testProperty "start never changes the zipper's contents" $ \z ->
      toList z == (toList (start z) :: List Integer)
  , testProperty "never have lefts after calling start" $ \z ->
      lefts (start z) == (Nil :: List Integer)
  ]

deletePullLeftTest :: TestTree
deletePullLeftTest =
  testGroup "deletePullLeft" [
    testCase "non-empty lefts" $ deletePullLeft (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [2,1] 3 [5,6,7])
  , testCase "empty lefts" $ deletePullLeft (zipper [] 1 [2,3,4]) @?= isNotZ
  ]

deletePullRightTest :: TestTree
deletePullRightTest =
  testGroup "deletePullRight" [
    testCase "non-empty rights" $ deletePullRight (zipper [3,2,1] 4 [5,6,7]) @?= isZ (zipper [3,2,1] 5 [6,7])
  , testCase "empty rights" $ deletePullRight (zipper [3,2,1] 4 []) @?= isNotZ
  ]

insertPushLeftTest :: TestTree
insertPushLeftTest =
  testGroup "insertPushLeft" [
    testCase "non-empty lefts" $
      insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7]) @?= zipper [4,3,2,1] 15 [5,6,7]
  , testCase "empty lefts" $
      insertPushLeft 15 (zipper [] 1 [2,3,4]) @?= zipper [1] 15 [2,3,4]
  , testProperty "deletePullLeft . insertPushLeft == id" $ \z i ->
      optional False (== (z :: ListZipper Integer)) $
      toOptional (deletePullLeft (insertPushLeft i z))
  ]

insertPushRightTest :: TestTree
insertPushRightTest =
  testGroup "insertPushRight" [
    testCase "non-empty rights" $
      insertPushRight 15 (zipper [3,2,1] 4 [5,6,7]) @?= zipper [3,2,1] 15 [4,5,6,7]
  , testCase "empty rights" $
      insertPushRight 15 (zipper [3,2,1] 4 []) @?= zipper [3,2,1] 15 [4]
  , testProperty "deletePullRight . insertPushRight == id" $ \z i ->
      optional False (== (z :: ListZipper Integer)) $
      toOptional (deletePullRight (insertPushRight i z))
  ]

applicativeTest :: TestTree
applicativeTest =
  testGroup "Applicative" [
    testProperty "pure produces infinite lefts" $ \a n ->
      (all . (==) <*> take (getPositive n) . lefts . pure) (a :: List Integer)
  , testProperty "pure produces infinite rights" $ \a n ->
      (all . (==) <*> take (getPositive n) . rights . pure) (a :: List Integer)
  , testCase "<*> applies functions to corresponding elements in zipper" $
      zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7] @?= zipper [5,12] 8 [15,24,12]
  ]

applicativeMaybeTest :: TestTree
applicativeMaybeTest =
  let is (MLZ (Full z)) = z
      is _       = error "MaybeListZipper's Applicative instances is busted"
      notZ       = isNotZ :: MaybeListZipper Integer
  in
    testGroup "Applicative (MaybeListZipper)" [
      testProperty "pure produces infinite lefts" $ \a n ->
        (all . (==) <*> take (getPositive n) . lefts . is . pure) (a :: List Integer)
    , testProperty "pure produces infinite rights" $ \a n ->
        (all . (==) <*> take (getPositive n) . rights . is . pure) (a :: List Integer)
    , testCase "isZ <*> isZ" $
        let z = isZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> isZ (zipper [3,2,1] 4 [5,6,7])
         in z @?= isZ (zipper [5,12] 8 [15,24,12])
    , testProperty "isNotZ <*> isZ" $
        let fs = (isNotZ :: MaybeListZipper (Integer -> Integer))
         in \z -> (fs <*> isZ z) == isNotZ
    , testProperty "isZ <*> isNotZ" $
        let a = (isNotZ :: MaybeListZipper Integer)
         in \z -> (isZ (getUnshowable z) <*> a) == (isNotZ :: MaybeListZipper Integer)
    , testCase "isNotZ <*> isNotZ" $
        isNotZ <*> isNotZ @?= notZ
    ]

extendTest :: TestTree
extendTest =
  testGroup "Extend" [
    testCase "zipper o' zippers" $
      let z = zipper [2,1] 3 [4,5]
          l = [zipper [1] 2 [3,4,5], zipper [] 1 [2,3,4,5]]
          r = [zipper [3,2,1] 4 [5], zipper [4,3,2,1] 5 []]
       in (id <<= z) @?= zipper l z r
  ]

extendMaybeTest :: TestTree
extendMaybeTest =
  testGroup "Extend (MaybeListZipper)" [
    testCase "isNotZ" $ (id <<= isNotZ) @?= (isNotZ :: MaybeListZipper (MaybeListZipper Integer))
  , testCase "isZ" $
      let z = isZ (zipper [2,1] 3 [4,5])
          l = isZ P.<$> [zipper [1] 2 [3,4,5], zipper [] 1 [2,3,4,5]]
          r = isZ P.<$> [zipper [3,2,1] 4 [5], zipper [4,3,2,1] 5 []]
       in (id <<= z) @?= isZ (zipper l z r)
  ]

comonadTest :: TestTree
comonadTest =
  testGroup "Comonad" [
    testCase "copure" $ copure (zipper [2,1] 3 [4,5]) @?= 3
  ]

traversableTest :: TestTree
traversableTest =
  testGroup "Traversable" [
    testProperty "All Full" $ \z ->
      traverse id (Full <$> z) == Full (z :: ListZipper Integer)
  , testCase "One Empty" $
      traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7]) @?= Empty
  ]

traversableMaybeTest :: TestTree
traversableMaybeTest =
  testGroup "Traversable (MaybeListZipper)" [
    testCase "isNotZ" $ traverse id isNotZ @?= (Full isNotZ :: Optional (MaybeListZipper Integer))
  , testProperty "isZ Full" $ \z ->
      traverse id (Full <$> isZ z) == Full (isZ (z :: ListZipper Integer))
  ]

optional :: b -> (a -> b) -> Optional a -> b
optional e _ Empty    = e
optional _ f (Full a) = f a
