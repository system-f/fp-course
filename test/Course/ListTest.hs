{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListTest where

import qualified Prelude               as P (fmap, foldr)

import           Test.QuickCheck       (Arbitrary (..))
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import           Course.Core
import           Course.List           (List (..), foldLeft, headOr, infinity,
                                        length, map, product, sum)

newtype AList a = AList { aList :: List a}
                  deriving Show

instance Arbitrary a => Arbitrary (AList a) where
  arbitrary = P.fmap ((AList . P.foldr (:.) Nil) :: [a] -> AList a) arbitrary

test_List :: TestTree
test_List =
  testGroup "List" [
    headOrTest
  , productTest
  , sumTest
  , lengthTest
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
      \x -> foldLeft (-) (sum $ aList x) (aList x) == 0
  ]

lengthTest :: TestTree
lengthTest =
  testGroup "length" [
    testCase "length 1..3" $ length (1 :. 2 :. 3 :. Nil) @?= 3
  , testProperty "summing a list of 1s is equal to its length" $
      \x -> sum (map (const 1) (aList x)) == length (aList x :: List Integer)
  ]
