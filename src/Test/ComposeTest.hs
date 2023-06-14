{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.ComposeTest (
  -- * Tests
    test_Compose
  , functorTest
  , applicativeTest
  , contravariantTest

  -- * Runner
  , test
  ) where

import           Test.Framework  (TestTree, testCase, testGroup, test, (@?=))

import           Course.Core
import           Course.List (length, List ((:.), Nil))
import           Course.Compose (Compose (Compose))
import           Course.Functor ((<$>))
import           Course.Applicative (pure, (<*>))
import           Course.Contravariant (Predicate (Predicate), (>$<), runPredicate)
import           Course.ExactlyOne (ExactlyOne (ExactlyOne), runExactlyOne)
import           Course.Optional (Optional (Full, Empty))

runCompose :: Compose f g a -> f (g a)
runCompose (Compose k) = k

test_Compose :: TestTree
test_Compose =
  testGroup "Compose" [
    functorTest
  , applicativeTest
  , contravariantTest
  ]

functorTest :: TestTree
functorTest =
  testGroup "Functor" [
    testCase "ExactlyOne Full" $
      (+1) <$> Compose (ExactlyOne (Full 2)) @?= Compose (ExactlyOne (Full 3))
  , testCase "ExactlyOne Empty" $
      (+1) <$> Compose (ExactlyOne Empty) @?= Compose (ExactlyOne Empty)
  ]

applicativeTest :: TestTree
applicativeTest =
  testGroup "Applicative" [
    testCase "pure" $
      pure 2 @?= Compose (ExactlyOne (Full 2))
  , testCase "ExactlyOne Full" $
      (+) <$> Compose (ExactlyOne (Full 1)) <*> Compose (ExactlyOne (Full 2)) @?=
        Compose (ExactlyOne (Full 3))
  ]

contravariantTest :: TestTree
contravariantTest =
  testGroup "Contravariant" [
    testCase "length even" $
      runPredicate
        (runExactlyOne (runCompose (length >$< Compose (ExactlyOne (Predicate even)))))
        (1 :. 2 :. 3 :. 4 :. Nil) @?= True
  ]
