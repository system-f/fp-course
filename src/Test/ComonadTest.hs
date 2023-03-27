{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Test.ComonadTest (
    -- * Tests
    test_Comonad,
    exactlyOneTest,
    fmapTest,

    -- * Runner
    test,
)
where

import Test.Framework (TestTree, test, testCase, testGroup, (@?=))

import Course.Comonad (copure, (<$$>))
import Course.Core
import Course.ExactlyOne (ExactlyOne (..))

test_Comonad :: TestTree
test_Comonad =
    testGroup
        "Comonad"
        [ exactlyOneTest
        , fmapTest
        ]

exactlyOneTest :: TestTree
exactlyOneTest =
    testCase "ExactlyOne" $ copure (ExactlyOne 7) @?= 7

fmapTest :: TestTree
fmapTest =
    testCase "<$$>" $
        ((+ 10) <$$> ExactlyOne 7) @?= ExactlyOne 17
