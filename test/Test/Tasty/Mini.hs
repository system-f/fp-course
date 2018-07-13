{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitPrelude       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Test.Tasty.Mini where

import qualified Test.Tasty as T
import           Test.Tasty.HUnit as T

import           Test.Mini        (Tester (..))


data TastyTester

instance Tester TastyTester T.TestName where
  data TestTree TastyTester = TTestTree {unTTestTree :: T.TestTree }
  data Assertion TastyTester = TAssertion { unTAssertion :: IO ()}

  testGroup n =
    TTestTree . T.testGroup n . fmap unTTestTree

  testCase n =
    TTestTree . T.testCase n . unTAssertion

  (@?=) a b = TAssertion (a T.@?= b)

  test = T.defaultMain . unTTestTree

tastyTest ::
  TestTree TastyTester
  -> IO ()
tastyTest = test

