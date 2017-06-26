{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ValidationTest where

import qualified Prelude as P(String, either, fmap)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Course.Core
import Course.Validation

instance Arbitrary a => Arbitrary (Validation a) where
  arbitrary = P.fmap (P.either Error Value) arbitrary

test_isError :: TestTree
test_isError =
  testGroup "isError" [
    testCase "true for errors" $
      isError (Error "Message") @?= True
  , testCase "false for values" $
      isError (Value 7) @?= False
  , testProperty "not the same as isValue" $
      \(x :: Validation Int) -> isError x /= isValue x
  ]

test_isValue :: TestTree
test_isValue =
  testGroup "isValue" [
    testCase "false for errors" $
      isValue (Error "Message") @?= False
  , testCase "false for values" $
      isValue (Value 7) @?= True
  , testProperty "not the same as isValue" $
      \(x :: Validation Int) -> isValue x /= isError x
  ]

test_mapValidation :: TestTree
test_mapValidation =
  testGroup "mapValidation" [
    testCase "errors unchanged" $
      mapValidation (+ 10) (Error "message") @?= Error "message"
  , testCase "values changed" $
      mapValidation (+ 10) (Value 7) @?= Value 17
  , testProperty "map with id causes no change" $
      \(x :: Validation Int) -> mapValidation id x == x
  ]

test_bindValidation :: TestTree
test_bindValidation =
  let
    f n = if even n then Value (n + 10) else Error "odd"
  in
    testGroup "bindValidation" [
      testCase "error unchanged" $
        bindValidation f (Error "message") @?= Error "message"
    , testCase "odd value" $
        bindValidation f (Value 7) @?= Error "odd"
    , testCase "even value" $
        bindValidation f (Value 8) @?= Value 18
    , testProperty "bind with Value causes no change" $
      \(x :: Validation Int) -> bindValidation Value x == x
    ]

test_valueOr :: TestTree
test_valueOr =
  testGroup "valueOr" [
    testCase "falls through for errors" $
      valueOr (Error "message") 3 @?= 3
  , testCase "unwraps values" $
      valueOr (Value 7) 3 @?= 7
  , testProperty "isValue or valueOr falls through" $
      \(x :: Validation Int) n -> isValue x || valueOr x n == n
  ]

test_errorOr :: TestTree
test_errorOr =
  testGroup "errorOr" [
    testCase "unwraps errors" $
      errorOr (Error "message") ("q" :: P.String) @?= "message"
  , testCase "falls through for values" $
      errorOr (Value 7) ("q" :: P.String) @?= "q"
  , testProperty "isError or errorOr falls through" $
      \(x :: Validation Int) n -> isError x || errorOr x n == n
  ]
