{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ApplicativeTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Course.Core
import Course.Applicative (lift2, lift3, pure, (<$>), (<*>))
import Course.Id (Id (..))
import Course.List (List (..), length, listh, product, sum)
import Course.Optional (Optional (..))
import Course.TestHelpers ((+:))

test_Applicative :: TestTree
test_Applicative =
  testGroup "Applicative" [
    haveFmapTest
  , idTest
  , listTest
  , optionalTest
  , functionTest
  , lift2Test
  ]

haveFmapTest :: TestTree
haveFmapTest =
  testGroup "<$>" [
    testCase "fmap Id" $
      (+: 1) <$> (Id 2) @?= Id (3 :: Integer)
  , testCase "fmap empty List" $
      (+: 1) <$> Nil @?= Nil
  , testCase "fmap List" $
      (+: 1) <$> listh [1,2,3] @?= listh [2,3,4]
  ]

idTest :: TestTree
idTest =
  testGroup "Id instance" [
    testProperty "pure == Id" $
      \(x :: Integer) -> pure x == Id x
  , testCase "Applying within Id" $
      Id (+: 10) <*> Id 8 @?= Id 18
  ]

listTest :: TestTree
listTest =
  testGroup "List instance" [
    testProperty "pure" $
      \x -> pure x == (x :. Nil :: List Integer)
  , testCase "<*>" $
      (+:1) :. (*2) :. Nil <*> listh [1,2,3] @?= listh [2,3,4,2,4,6]
  ]

optionalTest :: TestTree
optionalTest =
  testGroup "Optional instance" [
    testProperty "pure" $
      \(x :: Integer) -> pure x == Full x
  , testCase "Full <*> Full" $
      Full (+:8) <*> Full 7 @?= Full 15
  , testCase "Empty <*> Full" $
      Empty <*> Full "tilt" @?= (Empty :: Optional Integer)
  , testCase "Full <*> Empty" $
      Full (+:8) <*> Empty @?= Empty
  ]

functionTest :: TestTree
functionTest =
  testGroup "Function instance" [
    testCase "addition" $
      ((+:) <*> (+:10)) 3 @?= 16
  , testCase "more addition" $
      ((+:) <*> (+:5)) 3 @?= 11
  , testCase "even more addition" $
      ((+:) <*> (+:5)) 1 @?= 7
  , testCase "addition and multiplication" $
      ((*) <*> (+:10)) 3 @?= 39
  , testCase "more addition and multiplcation" $
      ((*) <*> (+:2)) 3 @?= 15
  , testProperty "pure" $
      \(x :: Integer) (y :: Integer) -> pure x y == x
  ]

lift2Test :: TestTree
lift2Test =
  testGroup "lift2" [
    testCase "+ over Id" $
      lift2 (+:) (Id 7) (Id 8) @?= Id 15
  , testCase "+ over List" $
      lift2 (+:) (listh [1,2,3]) (listh [4,5]) @?= listh [5,6,6,7,7,8]
  , testCase "+ over Optional - all full" $
      lift2 (+:) (Full 7) (Full 8) @?= Full 15
  , testCase "+ over Optional - first Empty" $
      lift2 (+:) Empty (Full 8) @?= Empty
  , testCase "+ over Optional - second Empty" $
      lift2 (+:) (Full 7) Empty @?= Empty
  , testCase "+ over functions" $
      lift2 (+) length sum (listh [4,5,6]) @?= 18
  ]

lift3Test :: TestTree
lift3Test =
  testGroup "lift3" [
    testCase "+ over Id" $
      lift3 (\a b c -> a +: b +: c) (Id 7) (Id 8) (Id 9) @?= Id 24
  , testCase "+ over List" $
      lift3 (\a b c -> a +: b +: c) (listh [1,2,3]) (listh [4,5]) (listh [6,7,8]) @?=
        listh [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
  , testCase "+ over Optional" $
      lift3 (\a b c -> a +: b +: c) (Full 7) (Full 8) (Full 9) @?= Full 24
  , testCase "+ over Optional - third Empty" $
      lift3 (\a b c -> a +: b +: c) (Full 7) (Full 8) Empty @?= Empty
  , testCase "+ over Optional - first Empty" $
      lift3 (\a b c -> a +: b +: c) Empty (Full 8) (Full 9) @?= Empty
  , testCase "+ over Optional - first and second Empty" $
      lift3 (\a b c -> a +: b +: c) Empty Empty (Full 9) @?= Empty
  , testCase "+ over functions" $
      lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6]) @?= 138
  ]
