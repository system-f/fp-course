{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Test.ComposeTest (
    -- * Tests
    test_Compose,
    functorTest,
    applicativeTest,

    -- * Runner
    test,
) where

import Test.Framework (TestTree, test, testCase, testGroup, (@?=))

import Course.Applicative (pure, (<*>))
import Course.Compose (Compose (Compose))
import Course.Core
import Course.ExactlyOne (ExactlyOne (ExactlyOne), runExactlyOne)
import Course.Functor ((<$>))
import Course.List (List (Nil, (:.)), length)
import Course.Optional (Optional (Empty, Full))

runCompose :: Compose f g a -> f (g a)
runCompose (Compose k) = k

test_Compose :: TestTree
test_Compose =
    testGroup
        "Compose"
        [ functorTest
        , applicativeTest
        ]

functorTest :: TestTree
functorTest =
    testGroup
        "Functor"
        [ testCase "ExactlyOne Full" $
            (+ 1) <$> Compose (ExactlyOne (Full 2)) @?= Compose (ExactlyOne (Full 3))
        , testCase "ExactlyOne Empty" $
            (+ 1) <$> Compose (ExactlyOne Empty) @?= Compose (ExactlyOne Empty)
        ]

applicativeTest :: TestTree
applicativeTest =
    testGroup
        "Applicative"
        [ testCase "pure" $
            pure 2 @?= Compose (ExactlyOne (Full 2))
        , testCase "ExactlyOne Full" $
            (+) <$> Compose (ExactlyOne (Full 1)) <*> Compose (ExactlyOne (Full 2))
                @?= Compose (ExactlyOne (Full 3))
        ]
