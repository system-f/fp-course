module L04.ListZipper.Tests where

import Data.Maybe (isJust)
import L03.Fluffy
import L04.ListZipper
import Test.Framework
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit                           hiding (Test, test)
import Test.QuickCheck
import Test.QuickCheck.Function

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
  testGroup "ListZipper"
    [
      testCase "furry on ListZipper" testcase_furryListZipper
    , testCase "furry on MaybeListZipper" testcase_furryMaybeListZipper
    , testProperty "fromList/toList roundtrip" prop_fromList_toList
    , testProperty "toMaybe" prop_toMaybe
    , testCase "withFocus (front)" testcase_withFocus_front
    , testCase "withFocus (middle)" testcase_withFocus_middle
    , testCase "setFocus (front)" testcase_setFocus_front
    , testCase "setFocus (middle)" testcase_setFocus_middle
    , testCase "hasLeft (yes)" testcase_hasLeft_yes
    , testCase "hasLeft (no)" testcase_hasLeft_no
    , testCase "hasRight (yes)" testcase_hasRight_yes
    , testCase "hasRight (no)" testcase_hasRight_no
    , testProperty "findLeft (no movement)" prop_findLeft_alreadyThere
    , testProperty "findLeft (unsatisfiable)" prop_findLeft_nowhere
    ]

testcase_furryListZipper ::
  Assertion
testcase_furryListZipper =
  furry (+1) (ListZipper [3,2,1] (4 :: Int) [5,6,7]) @?=
    (ListZipper [4,3,2] 5 [6,7,8])

testcase_furryMaybeListZipper ::
  Assertion
testcase_furryMaybeListZipper =
  furry (+1) (IsZ (ListZipper [3,2,1] (4 :: Int) [5,6,7])) @?=
    IsZ (ListZipper [4,3,2] 5 [6,7,8])

prop_fromList_toList ::
  [Int]
  -> Bool
prop_fromList_toList xs =
  xs == toList (fromList xs)

prop_toMaybe ::
  [Int]
  -> Bool
prop_toMaybe xs =
  if null xs then m == Nothing else m /= Nothing
  where
    m = toMaybe (fromList xs)

testcase_withFocus_front ::
  Assertion
testcase_withFocus_front =
  withFocus (+1) (ListZipper [] 0 [1]) @?= ListZipper [] 1 [1 :: Int]

testcase_withFocus_middle ::
  Assertion
testcase_withFocus_middle =
  withFocus (+1) (ListZipper [1,0] 2 [3,4]) @?= ListZipper [1,0] 3 [3,4 :: Int]

testcase_setFocus_front ::
  Assertion
testcase_setFocus_front =
  setFocus 1 (ListZipper [] 0 [1]) @?= ListZipper [] 1 [1 :: Int]

testcase_setFocus_middle ::
  Assertion
testcase_setFocus_middle =
  setFocus 1 (ListZipper [1,0] 2 [3,4]) @?= ListZipper [1,0] 1 [3,4 :: Int]

testcase_hasLeft_yes ::
  Assertion
testcase_hasLeft_yes =
  hasLeft (ListZipper [1,0] 2 [3,4 :: Int]) @?= True

testcase_hasLeft_no ::
  Assertion
testcase_hasLeft_no =
  hasLeft (ListZipper [] 0 [1,2 :: Int]) @?= False

testcase_hasRight_yes ::
  Assertion
testcase_hasRight_yes =
  hasRight (ListZipper [1,0] 2 [3,4 :: Int]) @?= True

testcase_hasRight_no ::
  Assertion
testcase_hasRight_no =
  hasRight (ListZipper [1,0 :: Int] 2 []) @?= False

prop_findLeft_alreadyThere ::
  [Int]
  -> Bool
prop_findLeft_alreadyThere xs =
  findLeft (const True) (fromList xs) == fromList xs

prop_findLeft_nowhere ::
  [Int]
  -> Bool
prop_findLeft_nowhere xs =
  case xs of
    [] -> findLeft (const False) IsNotZ == (IsNotZ :: MaybeListZipper Int)
    (x:xs') -> findLeft (const False) (IsZ (ListZipper [] x xs')) == IsNotZ
