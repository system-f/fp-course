{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.ContravariantTest (
  -- * Tests
    test_Contravariant
  , predicateTest
  , comparisonTest
  , swappedArrowTest
  , ignoreTest

  -- * Runner
  , test
  ) where

import           Test.Framework  (TestTree, testCase, testGroup, test, (@?=), testProperty)

import           Course.Core
import           Course.Contravariant ((>$<), (>$), Predicate (Predicate), runPredicate,
                                       Comparison (Comparison), runComparison,
                                       SwappedArrow (SwappedArrow), runSwappedArrow)
import           Course.List (length, (++), List (Nil, (:.)), listh)

test_Contravariant :: TestTree
test_Contravariant =
  testGroup "Contravariant" [
    predicateTest
  , comparisonTest
  , swappedArrowTest
  , ignoreTest
  ]


predicateTest :: TestTree
predicateTest =
  testGroup "Predicate" [
    testCase "even" $
      runPredicate ((+1) >$< Predicate even) 2 @?= False
  , testCase "even length" $
      runPredicate (length >$< Predicate even) (1 :. 2 :. Nil) @?= True
  ]

comparisonTest :: TestTree
comparisonTest =
  testGroup "Comparison" [
    testCase "show" $
      runComparison (show >$< Comparison compare) 2 12 @?= GT
  , testCase "id" $
      runComparison (id >$< Comparison compare) 2 12 @?= LT
  , testCase "length" $
      runComparison (length >$< Comparison compare) ('a' :. Nil) ('b' :. Nil) @?= EQ
  ]

swappedArrowTest :: TestTree
swappedArrowTest =
  testGroup "SwappedArrow" [
    testCase "length" $
      runSwappedArrow (length >$< SwappedArrow (+10)) (listh "hello") @?= 15
  , testCase "id" $
      runSwappedArrow (id >$< SwappedArrow (++ listh " world")) (listh "hello") @?= listh "hello world"
  ]

ignoreTest :: TestTree
ignoreTest =
  testGroup "Ignore" [
    testProperty "Ignore input value, always odd" $ \x ->
      runPredicate (3 >$ Predicate odd) (x :: Integer)
  , testProperty "Ignore input value, always even" $ \x ->
      not (runPredicate (4 >$ Predicate odd) (x :: Integer))
  ]
