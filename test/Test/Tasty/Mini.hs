{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitPrelude       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Test.Tasty.Mini where

import qualified Test.Tasty as T
import           Test.Tasty.HUnit as T

import           Test.Mini        (Tester (..), UnitTester (..))


newtype TastyAssertion =
  TA {getTA :: IO ()}

newtype TastyTree =
  TT {getTT :: T.TestTree}

instance Tester TastyTree T.TestName TastyAssertion where
  testGroup n =
    TT . T.testGroup n . fmap getTT

  testCase n =
    TT . T.testCase n . getTA

  test = T.defaultMain . getTT

instance UnitTester TastyAssertion where
  (@?=) = (TA .) . (T.@?=)


tastyTest ::
  TastyTree
  -> IO ()
tastyTest = test

