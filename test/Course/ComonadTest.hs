{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ComonadTest
  (
  -- * Tests
    test_Comonad
  , exactlyOneTest
  , fmapTest
  )
  where

import           Test.Course.Mini  (TestTree, testCase, testGroup, (@?=))

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
