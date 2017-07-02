{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListTest where

import           Control.Applicative   (liftA3)
import qualified Prelude               as P (fmap, foldr, (<$>), (>>=))

import           Test.QuickCheck       (Arbitrary (..), Gen, forAll)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import           Course.Core
import           Course.List           (List (..), filter, flatMap, flatten,
                                        foldLeft, headOr, infinity, length,
                                        listh, map, product, sum, (++))

test_List :: TestTree
test_List =
  testGroup "List" [
    headOrTest
  , productTest
  , sumTest
  , lengthTest
  , mapTest
  , filterTest
  , appendTest
  , flattenTest
  ]

headOrTest :: TestTree
headOrTest =
  testGroup "headOr" [
    testCase "headOr on non-empty list" $ headOr 3 (1 :. 2 :. Nil) @?= 1
  , testCase "headOr on empty list" $ headOr 3 Nil @?= 3
  , testProperty "headOr on infinity always 0" $ \x -> x `headOr` infinity == 0
  , testProperty "headOr on empty list always the default" $ \x -> x `headOr` Nil == (x :: Integer)
  ]

productTest :: TestTree
productTest =
  testGroup "productTest" [
    testCase "product of empty list" $ product Nil @?= 1
  , testCase "product of 1..3" $ product (1 :. 2 :. 3 :. Nil) @?= 6
  , testCase "product of 1..4" $ product (1 :. 2 :. 3 :. 4 :. Nil) @?= 24
  ]

sumTest :: TestTree
sumTest =
  testGroup "sum" [
    testCase "sum 1..3" $ sum (1 :. 2 :. 3 :. Nil) @?= 6
  , testCase "sum 1..4" $ sum (1 :. 2 :. 3 :. 4 :. Nil) @?= 10
  , testProperty "subtracting each element in a list from its sum is always 0" $
      forAll genList (\x -> foldLeft (-) (sum x) x == 0)
  ]

lengthTest :: TestTree
lengthTest =
  testGroup "length" [
    testCase "length 1..3" $ length (1 :. 2 :. 3 :. Nil) @?= 3
  , testProperty "summing a list of 1s is equal to its length" $
      forAll genIntegerList (\x -> sum (map (const 1) x) == length x)
  ]

mapTest :: TestTree
mapTest =
  testGroup "map" [
    testCase "add 10 on list" $
      map (+10) (1 :. 2 :. 3 :. Nil) @?= (11 :. 12 :. 13 :. Nil)
  , testProperty "headOr after map" $
      \x -> headOr (x :: Integer) (map (+1) infinity) == 1
  , testProperty "map id is id" $
      forAll genIntegerList (\x -> map id x == x)
  ]

filterTest :: TestTree
filterTest =
  testGroup "filter" [
    testCase "filter even" $
      filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) @?= (2 :. 4 :. Nil)
  , testProperty "filter (const True) is identity (headOr)" $
      \x -> headOr x (filter (const True) infinity) == 0
  , testProperty "filter (const True) is identity" $
      forAll genIntegerList (\x -> filter (const True) x == x)
  , testProperty "filter (const False) is the empty list" $
      forAll genIntegerList (\x -> filter (const False) x == Nil)
  ]

appendTest :: TestTree
appendTest =
  testGroup "(++)" [
    testCase "(1..6)" $
      (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil) @?= listh [1,2,3,4,5,6]
  , testProperty "append empty to infinite" $
      \x -> headOr x (Nil ++ infinity) == 0
  , testProperty "append anything to infinity" $
       forAll genIntegerAndList (\(x, y) -> headOr x (y ++ infinity) == headOr 0 y)
  , testProperty "associativity" $
      forAll (liftA3 (,,) genIntegerList genIntegerList genIntegerList)
             (\(x,y,z) -> (x ++ y) ++ z == x ++ (y ++ z))
  , testProperty "append to empty list" $
      forAll genIntegerList (\x -> x ++ Nil == x)
  ]

flattenTest :: TestTree
flattenTest =
  testGroup "flatten" [
    testCase "(1..9)" $
      flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) @?= listh [1,2,3,4,5,6,7,8,9]
  , testProperty "flatten (infinity :. y)" $
      forAll genIntegerAndList (\(x, y) -> headOr x (flatten (infinity :. y :. Nil)) == 0)
  , testProperty "flatten (y :. infinity)" $
      forAll genIntegerAndList (\(x, y) -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y)
  , testProperty "sum of lengths == sum of flattened" $
      forAll genListOfLists (\x -> sum (map length x) == length (flatten x))
  ]

flatMapTest :: TestTree
flatMapTest =
  testGroup "flatMap" [
    testCase "lists of Integer" $
      flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil) @?= listh [1,2,3,2,3,4,3,4,5]
  , testProperty "flatMap id flattens a list of lists" $
      forAll genIntegerAndList (\(x, y) -> headOr x (flatMap id (infinity :. y :. Nil)) == 0)
  , testProperty "flatMap id on a list of lists take 2" $
      forAll genIntegerAndList (\(x, y) -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y)
  , testProperty "flatMap id == flatten" $
      forAll genListOfLists (\x -> flatMap id x == flatten x)
  ]

-- Use generator functions with `forAll` rather than orphans and/or newtype wrappers
genList :: Arbitrary a => Gen (List a)
genList = P.fmap ((P.foldr (:.) Nil) :: [a] -> List a) arbitrary

genIntegerList :: Gen (List Integer)
genIntegerList = genList

genIntegerAndList :: Gen (Integer, List Integer)
genIntegerAndList = arbitrary P.>>= (P.<$> genIntegerList) . (,)

genListOfLists :: Gen (List (List Integer))
genListOfLists = P.fmap (P.fmap listh) (genList :: (Gen (List [Integer])))
