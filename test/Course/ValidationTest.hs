{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Course.ValidationTest (
  -- * Tests
    test_Validation
  , isErrorTest
  , isValueTest
  , mapValidationTest
  , bindValidationTest
  , valueOrTest
  , errorOrTest

  -- * Course test runner
  , courseTest
  ) where

import           Data.Maybe        (maybe)
import qualified Prelude           as P
import           Test.Course.Mini  (courseTest)
import           Test.Mini         (Arbitrary (shrink),
                                    Gen (GenA, GenInt, GenMaybe, GenString),
                                    MiniTestTree, Testable (B, Fn), testCase,
                                    testGroup, testProperty, (@?=))

import           Course.Core
import           Course.Validation

test_Validation :: MiniTestTree
test_Validation =
  testGroup "Validation" [
    isErrorTest
  , isValueTest
  , mapValidationTest
  , bindValidationTest
  , valueOrTest
  , errorOrTest
  ]

isErrorTest :: MiniTestTree
isErrorTest =
  testGroup "isError" [
    testCase "true for errors" $
      isError (Error "Message") @?= True
  , testCase "false for values" $
      isError (Value "7") @?= False
  , testProperty "not the same as isValue" . Fn genValidationInt $
      \(x :: Validation Int) -> B (isError x /= isValue x)
  ]

isValueTest :: MiniTestTree
isValueTest =
  testGroup "isValue" [
    testCase "false for errors" $
      isValue (Error "Message") @?= False
  , testCase "false for values" $
      isValue (Value "7") @?= True
  , testProperty "not the same as isValue" . Fn genValidationInt $
      \(x :: Validation Int) -> B (isValue x /= isError x)
  ]

mapValidationTest :: MiniTestTree
mapValidationTest =
  testGroup "mapValidation" [
    testCase "errors unchanged" $
      mapValidation (+ 10) (Error "message") @?= Error "message"
  , testCase "values changed" $
      mapValidation (+ 10) (Value 7) @?= Value 17
  , testProperty "map with id causes no change" . Fn genValidationInt $
      \(x :: Validation Int) -> B (mapValidation id x == x)
  ]

bindValidationTest :: MiniTestTree
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
    , testProperty "bind with Value causes no change" . Fn genValidationInt $
      \(x :: Validation Int) -> B $ bindValidation Value x == x
    ]

valueOrTest :: MiniTestTree
valueOrTest =
  testGroup "valueOr" [
    testCase "falls through for errors" $
      valueOr (Error "message") "foo" @?= "foo"
  , testCase "unwraps values" $
      valueOr (Value "foo") "bar" @?= "foo"
  , testProperty "isValue or valueOr falls through" . Fn genValidationInt $
      \(x :: Validation Int) -> Fn GenInt $ \n -> B $ isValue x || valueOr x n == n
  ]

errorOrTest :: MiniTestTree
errorOrTest =
  testGroup "errorOr" [
    testCase "unwraps errors" $
      errorOr (Error "message") "q" @?= "message"
  , testCase "falls through for values" $
      errorOr (Value (7 :: Integer)) "q" @?= "q"
  , testProperty "isError or errorOr falls through" . Fn genValidationInt $
      \(x :: Validation Int) -> Fn GenString $ \s -> B (isError x || errorOr x s == s)
  ]

genValidationInt ::
  forall t g.
  Arbitrary t g
  => Gen t (Validation Int)
genValidationInt =
  let
    gmi = GenMaybe GenInt
    f = maybe (Error "no value") Value
    shrink' = \case
      (Value n) -> Value P.<$> shrink (GenInt :: Gen t Int) n
      e@(Error _) -> [e]
  in
    GenA gmi f shrink'
