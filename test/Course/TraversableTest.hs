{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.TraversableTest where

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))

import           Course.Compose        (Compose (..))
import           Course.Core
import           Course.ExactlyOne     (ExactlyOne (..))
import           Course.Functor
import           Course.List           (List (..), listh)
import           Course.Optional       (Optional (..))
import           Course.Traversable

test_Traversable :: TestTree
test_Traversable =
  testGroup "Traversable" [
    listTest
  , exactlyOneTest
  , optionalTest
  , sequenceATest
  , composeTest
  , productFunctorTest
  , productTraversableTest
  , coProductFunctorTest
  , coProductTraversableTest
  ]

listTest :: TestTree
listTest =
  testGroup "listTest" [
    testCase "traverse on empty list" $
      traverse (\a -> Full (a * 2)) (Nil :: List Int) @?= Full Nil
  , testCase "traverse on non-empty list" $
      traverse (\a -> Full (a * 2)) (listh [1, 2, 3]) @?= Full (listh [2, 4, 6])
  ]

exactlyOneTest :: TestTree
exactlyOneTest =
  testGroup "exactlyOneTest" [
    testCase "traverse on ExactlyOne" $
      traverse (\a -> Full (a * 2)) (ExactlyOne 3) @?= Full (ExactlyOne 6)
  ]

optionalTest :: TestTree
optionalTest =
  testGroup "optionalTest" [
    testCase "traverse on Empty" $
      traverse (\a -> ExactlyOne (a * 2)) Empty @?= ExactlyOne Empty
  , testCase "traverse on Full" $
      traverse (\a -> ExactlyOne (a * 2)) (Full 5) @?= ExactlyOne (Full 10)
  ]

sequenceATest :: TestTree
sequenceATest =
  testGroup "sequenceATest" [
    testCase "on List over ExactlyOne" $
      sequenceA (listh [ExactlyOne 7, ExactlyOne 8, ExactlyOne 9]) @?= ExactlyOne (listh [7,8,9])
  , testCase "on Optional over ExactlyOne" $
      sequenceA (Full (ExactlyOne 7)) @?= ExactlyOne (Full 7)
  , testCase "on Optional over function" $
      sequenceA (Full (*10)) 6 @?= Full 60
  ]

composeTest :: TestTree
composeTest =
  testGroup "composeTest" [
    testCase "traverse on Compose Optional List Int" $
      traverse (\a -> ExactlyOne (a * 2)) cfli @?= ExactlyOne traversedCfli
  ,  testCase "traverse on Compose List ExactlyOne Int" $
      traverse (\a -> Full (a * 2)) clei @?= Full traversedClei
  ]
  where
    cfli = Compose fullListOfInts
    traversedCfli = Compose $ (*2) `fmap2` fullListOfInts
    clei = Compose listOfExactlyOnes
    traversedClei = Compose $ (*2) `fmap2` listOfExactlyOnes
    fullListOfInts = Full (listh [1, 2, 3])
    listOfExactlyOnes = listh [ExactlyOne 1, ExactlyOne 2, ExactlyOne 3]
    fmap2 f = ((f <$>) <$>)

productFunctorTest :: TestTree
productFunctorTest =
  testGroup "productFunctorTest" [
    testCase "fmap on Product Optional List Int" $
      (*2) <$> Product (Full 4) listOfInts @?= Product (Full 8) ((*2) <$> listOfInts)
  , testCase "fmap on Product ExactlyOne Optional Int" $
      (*2) <$> Product (ExactlyOne 4) Empty @?= Product (ExactlyOne 8) Empty
  ]
  where
    listOfInts = listh [1, 2, 3]

productTraversableTest :: TestTree
productTraversableTest =
  testGroup "productTraversableTest" [
    testCase "traverse on Product Optional List Int" $
      traverse (\a -> ExactlyOne (a*2)) product @?= ExactlyOne productTimesTwo
  ]
  where
    listOfInts = listh [1, 2, 3]
    product = Product (Full 4) listOfInts
    productTimesTwo = Product (Full 8) ((*2) <$> listOfInts)

coProductFunctorTest :: TestTree
coProductFunctorTest =
  testGroup "coProductFunctorTest" [
    testCase "fmap on InL Optional Int" $
      (*2) <$> inL @?= inLTimesTwo
  , testCase "fmap on InR ExactlyOne Int" $
      (*2) <$> inR @?= inRTimesTwo
  ]
  where
    inL, inLTimesTwo :: Coproduct Optional List Int
    inL = InL (Full 4)
    inLTimesTwo = InL (Full 8)
    inR, inRTimesTwo :: Coproduct Optional List Int
    inR = InR listOfInts
    inRTimesTwo = InR ((*2) <$> listOfInts)
    listOfInts = listh [1, 2, 3]

coProductTraversableTest :: TestTree
coProductTraversableTest =
  testGroup "coProductTraversableTest" [
    testCase "traverse on InL Optional Int" $
      traverse (\a -> ExactlyOne (a*2)) inL @?= ExactlyOne inLTimesTwo
  , testCase "traverse on InR List Int" $
      traverse (\a -> Full (a*2)) inR @?= Full inRTimesTwo
  ]
  where
    inL, inLTimesTwo :: Coproduct Optional List Int
    inL = InL (Full 4)
    inLTimesTwo = InL (Full 8)
    inR, inRTimesTwo :: Coproduct Optional List Int
    inR = InR listOfInts
    inRTimesTwo = InR ((*2) <$> listOfInts)
    listOfInts = listh [1, 2, 3]

