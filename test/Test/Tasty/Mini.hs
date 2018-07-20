{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitPrelude            #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Test.Tasty.Mini where

import           Course.Validation

import qualified Test.QuickCheck       as Q
import qualified Test.Tasty            as T
import qualified Test.Tasty.HUnit      as T
import qualified Test.Tasty.QuickCheck as T

import           Test.Mini             (Gen (..), PropertyTester (..), Arbitrary (..),
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

-- newtype QGen a =
--   QGen (Q.Gen a)
--   deriving

newtype QGen a =
  QGen (Q.Gen a)
  deriving (Functor, Applicative, Monad)

instance Gen TastyTree QGen Int where
  gen = QGen Q.arbitrary
  shrink = const Q.shrink

instance PropertyTester TastyTree QGen T.TestName where
  testProperty n = TT . T.testProperty n . T.property

instance T.Testable (Testable TastyTree QGen) where
  property = \case
    B b ->
      T.property b
    Fn (f :: a -> Testable TastyTree QGen) ->
      let
        shrink' = shrink (undefined :: p TastyTree)
        (QGen gen') = gen
      in
        Q.forAllShrink gen' shrink' f


tastyTest ::
  TastyTree
  -> IO ()
tastyTest = test

