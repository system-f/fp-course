{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ComonadTest
  (
  -- * Tests
    test_Comonad
  , exactlyOneTest
  , fmapTest

  -- * Runner
  , test
  )
  where

import           Test.Framework  (TestTree, testCase, testGroup, test, (@?=))

import           Course.Comonad    (copure, (<$$>))
import           Course.Core
import           Course.ExactlyOne (ExactlyOne (..))

test_Comonad :: TestTree
test_Comonad =
  testGroup "Comonad" [
    exactlyOneTest
  , fmapTest
  ]

exactlyOneTest :: TestTree
exactlyOneTest =
  testCase "ExactlyOne" $ copure (ExactlyOne 7) @?= 7

fmapTest :: TestTree
fmapTest =
  testCase "<$$>" $
    ((+10) <$$> ExactlyOne 7) @?= ExactlyOne 17
