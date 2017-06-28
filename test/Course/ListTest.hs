{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListTest where

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Course.Core

test_List :: TestTree
test_List =
  testGroup "List" [
    testCase "placeholder" $ 1 @?= 1
  ]
