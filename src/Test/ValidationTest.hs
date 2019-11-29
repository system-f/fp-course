{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.ValidationTest (
  -- * Tests
    test_Validation
  , isErrorTest
  , isValueTest
  , mapValidationTest
  , bindValidationTest
  , valueOrTest
  , errorOrTest

  -- * Runner
  , test
  ) where

import           Data.Maybe        (maybe)
import qualified Prelude           as P
import           Test.Framework    (TestTree, (@?=), testCase, testGroup,
                                    testProperty, test)

import           Course.Core
import           Course.Validation

test_Validation :: TestTree
test_Validation =
  testGroup "Validation" [
    isErrorTest
  , isValueTest
  , mapValidationTest
  , bindValidationTest
  , valueOrTest
  , errorOrTest
  ]

isErrorTest :: TestTree
isErrorTest =
  testGroup "isError" [
    testCase "true for errors" $
      isError (Error "Message") @?= True
  , testCase "false for values" $
      isError (Value "7") @?= False
  , testProperty "not the same as isValue" $ \x ->
      isError x /= isValue (x :: Validation Integer)
  ]

isValueTest :: TestTree
isValueTest =
  testGroup "isValue" [
    testCase "false for errors" $
      isValue (Error "Message") @?= False
  , testCase "false for values" $
      isValue (Value "7") @?= True
  , testProperty "not the same as isValue" $ \x ->
      isValue x /= isError (x :: Validation Integer)
  ]

mapValidationTest :: TestTree
mapValidationTest =
  testGroup "mapValidation" [
    testCase "errors unchanged" $
      mapValidation (+ 10) (Error "message") @?= Error "message"
  , testCase "values changed" $
      mapValidation (+ 10) (Value 7) @?= Value 17
  , testProperty "map with id causes no change" $ \x ->
      mapValidation id x == (x :: Validation Integer)
  ]

bindValidationTest :: TestTree
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
    , testProperty "bind with Value causes no change" $ \x ->
        bindValidation Value x == (x :: Validation Integer)
    ]

valueOrTest :: TestTree
valueOrTest =
  testGroup "valueOr" [
    testCase "falls through for errors" $
      valueOr (Error "message") "foo" @?= "foo"
  , testCase "unwraps values" $
      valueOr (Value "foo") "bar" @?= "foo"
  , testProperty "isValue or valueOr falls through" $ \x n ->
      isValue x || valueOr x n == (n :: Integer)
  ]

errorOrTest :: TestTree
errorOrTest =
  testGroup "errorOr" [
    testCase "unwraps errors" $
      errorOr (Error "message") "q" @?= "message"
  , testCase "falls through for values" $
      errorOr (Value (7 :: Integer)) "q" @?= "q"
  , testProperty "isError or errorOr falls through" $ \x s ->
      isError (x :: Validation Integer) || errorOr x s == s
  ]
