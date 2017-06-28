{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.FunctorTest where

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Course.Core
import           Course.Functor   ((<$>))
import           Course.Id        (Id (..))
import           Course.List      (List (..))

test_Functor :: TestTree
test_Functor =
  testGroup "Functor" [
    idTest
  , listTest
  ]

idTest :: TestTree
idTest =
  testCase "Id" $ (+1) <$> Id 2 @?= Id 3

listTest :: TestTree
listTest =
  testGroup "List" [
    testCase "empty list" $
      (+1) <$> Nil @?= Nil
  ]

-- Test :: TestTree
-- Test =
--   testGroup "" [

--   ]
