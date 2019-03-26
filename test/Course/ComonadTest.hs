{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ComonadTest
  (
  -- * Tests
    test_Comonad
  , exactlyOneTest
  , fmapTest

  -- * Course test runner
  , courseTest
  )
  where

import           Test.Course.Mini  (courseTest)
import           Test.Mini         (MiniTestTree, testCase, testGroup, (@?=))

import           Course.Comonad    (copure, (<$$>))
import           Course.Core
import           Course.ExactlyOne (ExactlyOne (..))

test_Comonad :: MiniTestTree
test_Comonad =
  testGroup "Comonad" [
    exactlyOneTest
  , fmapTest
  ]

exactlyOneTest :: MiniTestTree
exactlyOneTest =
  testCase "ExactlyOne" $ copure (ExactlyOne 7) @?= 7

fmapTest :: MiniTestTree
fmapTest =
  testCase "<$$>" $
    ((+10) <$$> ExactlyOne 7) @?= ExactlyOne 17
