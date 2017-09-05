{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ExtendTest where


import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (ExactlyOne))
import           Course.List       (List (..), length)

import           Course.Extend     ((<<=))

test_Extend :: TestTree
test_Extend =
  testGroup "Extend" [
    exactlyOneTest
  , listTest
  ]

exactlyOneTest :: TestTree
exactlyOneTest =
  testCase "ExactlyOne instance" $
    (id <<= ExactlyOne 7) @?= ExactlyOne (ExactlyOne 7)

listTest :: TestTree
listTest =
  testGroup "List" [
    testCase "length" $
      (length <<= ('a' :. 'b' :. 'c' :. Nil)) @?= (3 :. 2 :. 1 :. Nil)
  ]
