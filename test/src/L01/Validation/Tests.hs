module L01.Validation.Tests where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import L01.Validation
import Test.QuickCheck
import Test.QuickCheck.Function

instance Arbitrary a => Arbitrary (Validation a) where
  arbitrary =
    fmap (either Error Value) arbitrary

main :: 
  IO ()
main = 
  defaultMain [test]

test :: 
  Test
test =  
  testGroup "Validation"
    [
      testProperty "isError is not equal to isValue" prop_isError_isValue
    , testProperty "valueOr produces or isValue" prop_valueOr
    , testProperty "errorOr produces or isError" prop_errorOr
    , testProperty "mapValidation maps" prop_map
    ]

prop_isError_isValue ::
  Validation Int
  -> Bool
prop_isError_isValue x =
  isError x /= isValue x

prop_valueOr ::
  Validation Int
  -> Int
  -> Bool
prop_valueOr x n =
  isValue x || valueOr x n == n
  
prop_errorOr ::
  Validation Err
  -> Err
  -> Bool
prop_errorOr x e =
  isError x || errorOr x e == e

prop_map ::
  Validation Int
  -> Fun Int Int
  -> Int
  -> Bool
prop_map x (Fun _ f) n =
  f (valueOr x n) == valueOr (mapValidation f x) (f n)