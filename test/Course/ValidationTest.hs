{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ValidationTest where

import qualified Prelude           as P (either, fmap)
import           Test.Mini         (MiniTestTree,
                                    Tester (..), UnitTester (..), Arbitrary (..), Testable (..))

import           Course.Core
import           Course.Validation

-- instance Arbitrary a => Arbitrary (Validation a) where
--   arbitrary = P.fmap (P.either Error Value) arbitrary

type ValidationTestTree =
  forall t name.
  ( Arbitrary t name (Validation Int)
  , Arbitrary t name Int
  , Arbitrary t name Err
  , Arbitrary t name ()
  )
  => MiniTestTree t

test_Validation :: ValidationTestTree
test_Validation =
  testGroup "Validation" [
    isErrorTest
  , isValueTest
  , mapValidationTest
  , bindValidationTest
  , valueOrTest
  , errorOrTest
  ]

isErrorTest :: ValidationTestTree
isErrorTest =
  testGroup "isError" [
    testCase "true for errors" $
      isError (Error "Message") @?= True
  , testCase "false for values" $
      isError (Value "7") @?= False
  , testProperty "not the same as isValue" . Fn $
      \(x :: Validation Int) -> B (isError x /= isValue x)
  ]

isValueTest :: ValidationTestTree
isValueTest =
  testGroup "isValue" [
    testCase "false for errors" $
      isValue (Error "Message") @?= False
  , testCase "false for values" $
      isValue (Value "7") @?= True
  , testProperty "not the same as isValue" . Fn $
      \(x :: Validation Int) -> B (isValue x /= isError x)
  ]

mapValidationTest :: ValidationTestTree
mapValidationTest =
  testGroup "mapValidation" [
    testCase "errors unchanged" $
      mapValidation (+ 10) (Error "message") @?= Error "message"
  , testCase "values changed" $
      mapValidation (+ 10) (Value 7) @?= Value 17
  , testProperty "map with id causes no change" . Fn $
      \(x :: Validation Int) -> B (mapValidation id x == x)
  ]

bindValidationTest :: ValidationTestTree
bindValidationTest =
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
    , testProperty "bind with Value causes no change" . Fn $
      \(x :: Validation Int) -> B $ bindValidation Value x == x
    ]

valueOrTest :: ValidationTestTree
valueOrTest =
  testGroup "valueOr" [
    testCase "falls through for errors" $
      valueOr (Error "message") "foo" @?= "foo"
  , testCase "unwraps values" $
      valueOr (Value "foo") "bar" @?= "foo"
  , testProperty "isValue or valueOr falls through" . Fn $
      \(x :: Validation Int) -> Fn $ \n -> B $ isValue x || valueOr x n == n
  ]

errorOrTest :: ValidationTestTree
errorOrTest =
  testGroup "errorOr" [
    testCase "unwraps errors" $
      errorOr (Error "message") "q" @?= "message"
  , testCase "falls through for values" $
      errorOr (Value (7 :: Integer)) "q" @?= "q"
  , testProperty "isError or errorOr falls through" . Fn $
      \(x :: Validation Int) -> Fn $ \n -> B (isError x || errorOr x n == n)
  ]
