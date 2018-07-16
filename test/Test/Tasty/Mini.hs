{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitPrelude       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Test.Tasty.Mini where

import qualified Test.Tasty as T
import           Test.Tasty.HUnit as T

import           Test.Mini        (Tester (..), UnitTester (..))


data TastyTester

newtype TastyAssertion =
  TA {getTA :: IO ()}

instance Tester TastyTester T.TestName TastyAssertion where
  data TestTree TastyTester = TTestTree {unTTestTree :: T.TestTree }

  testGroup n =
    TTestTree . T.testGroup n . fmap unTTestTree

  testCase n =
    TTestTree . T.testCase n . getTA

  test = T.defaultMain . unTTestTree

instance UnitTester TastyAssertion where
  (@?=) = (TA .) . (T.@?=)


tastyTest ::
  TestTree TastyTester
  -> IO ()
tastyTest = test

