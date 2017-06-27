{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ApplicativeTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Course.Core
import Course.Applicative (pure, (<$>), (<*>))
-- import Course.Functor hiding ((<$>))
import Course.Id (Id (..))
import Course.List (List (..))
import Course.Optional (Optional (..))
import Course.TestHelpers ((+:))
-- import qualified Prelude as P(fmap, return, (>>=))

test_Applicative :: TestTree
test_Applicative =
  testGroup "Applicative" [
    haveFmapTest
  , idTest
  , listTest
  , optionalTest
  , functionTest
  ]

haveFmapTest :: TestTree
haveFmapTest =
  testGroup "<$>" [
    testCase "fmap Id" $
      (+: 1) <$> (Id 2) @?= Id (3 :: Integer)
  , testCase "fmap empty List" $
      (+: 1) <$> Nil @?= Nil
  , testCase "fmap List" $
      (+: 1) <$> (1 :. 2 :. 3 :. Nil) @?= (2 :. 3 :. 4 :. Nil)
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
      (+:1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil @?=
        2 :. 3 :. 4 :. 2 :. 4 :. 6 :. Nil
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
