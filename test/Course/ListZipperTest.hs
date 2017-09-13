{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListZipperTest where


import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))

import           Course.Core
import           Course.Functor    ((<$>))
import           Course.List       (List (..))
import           Course.ListZipper (MaybeListZipper (..), fromList, zipper)

test_ListZipper :: TestTree
test_ListZipper =
  testGroup "ListZipper" [
    functorTest
  , functorMaybeTest
  , fromListTest
  ]

functorTest :: TestTree
functorTest =
  testCase "ListZipper (<$>)" $
    (+1) <$> (zipper [3,2,1] 4 [5,6,7]) @?= zipper [4,3,2] 5 [6,7,8]

functorMaybeTest :: TestTree
functorMaybeTest =
  testCase "MaybeListZipper (<$>)" $
    (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7])) @?= IsZ (zipper [4,3,2] 5 [6,7,8])

fromListTest :: TestTree
fromListTest =
  testGroup "fromList" [
    testCase "non-empty" $ fromList (1 :. 2 :. 3 :. Nil) @?= IsZ (zipper [] 1 [2,3])
  ]
