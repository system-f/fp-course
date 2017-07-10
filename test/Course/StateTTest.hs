{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.StateTTest where

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))

import           Course.Applicative    (pure)
import           Course.Core
import           Course.Functor        ((<$>))
import           Course.List           (List (..))
import           Course.StateT         (StateT (..))

test_StateT :: TestTree
test_StateT =
  testGroup "StateT" [
    functorTest
  ]

functorTest :: TestTree
functorTest =
  testCase "<$>" $
    runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0 @?= ((3,0) :. Nil)
