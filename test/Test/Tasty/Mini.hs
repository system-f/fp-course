{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitPrelude       #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Test.Tasty.Mini where

import           Course.Validation

import qualified Test.QuickCheck       as Q
import qualified Test.Tasty            as T
import qualified Test.Tasty.HUnit      as T
import qualified Test.Tasty.QuickCheck as T

import           Test.Mini             (Arbitrary (..),
                                        Testable (..), Tester (..),
                                        UnitTester (..))


newtype TastyAssertion =
  TA {getTA :: IO ()}

newtype TastyTree =
  TT {getTT :: T.TestTree}

instance Tester TastyTree T.TestName where
  testGroup n =
    TT . T.testGroup n . fmap getTT

  test = T.defaultMain . getTT

instance UnitTester TastyTree T.TestName TastyAssertion where
  testCase n =
    TT . T.testCase n . getTA

  (@?=) = (TA .) . (T.@?=)

instance T.Arbitrary (Validation Int) where
  arbitrary = Value <$> Q.arbitrary

instance (T.Arbitrary a, Show a) => T.Testable (Testable TastyTree a) where
  property (B b) = T.property b
  property (Fn f) = undefined --T.property f

-- instance Arbitrary TastyTree T.TestName (Testable )

instance Arbitrary TastyTree T.TestName (Validation Int) where
  testProperty n (Fn f) = undefined
    --TT $ T.testProperty n f

instance Arbitrary TastyTree T.TestName Err where
  testProperty n (Fn f) = undefined
    --TT $ T.testProperty n f

instance Arbitrary TastyTree T.TestName Int where
  testProperty n (Fn f) = undefined
    --TT $ T.testProperty n f


tastyTest ::
  TastyTree
  -> IO ()
tastyTest = test

