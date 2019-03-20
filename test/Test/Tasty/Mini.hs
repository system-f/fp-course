{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitPrelude       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.Tasty.Mini where

import qualified Test.QuickCheck       as Q
import qualified Test.Tasty            as T
import qualified Test.Tasty.HUnit      as T
import qualified Test.Tasty.QuickCheck as T

import           Test.Mini             (Arbitrary (gen, shrink), Gen (GenA, GenAB, GenBool, GenInt, GenList, GenMaybe, GenString),
                                        PropertyTester (testProperty),
                                        Testable (B, Fn),
                                        Tester (test, testGroup),
                                        UnitTester (testCase, (@?=)))


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

data QGen a =
  QGen
  { qGen    :: Q.Gen a
  , qShrink :: a -> [a]
  }

instance Arbitrary TastyTree QGen where
  gen = \case
    GenBool -> QGen Q.arbitrary Q.shrink
    GenInt -> QGen Q.arbitrary Q.shrink
    GenString -> QGen Q.arbitrary Q.shrink
    GenA (foo :: Gen TastyTree a) (f :: a -> b) (s :: b -> [b]) ->
      let
        QGen qg _ = gen foo
      in
        QGen (f <$> qg) s
    gm@(GenMaybe (g :: Gen TastyTree a)) ->
      let
        QGen qg _ = gen g
        gen' = Q.oneof [Just <$> qg, pure Nothing]
      in
        QGen gen' (shrink gm)
    gl@(GenList (g :: Gen TastyTree a)) ->
      let
        QGen qg _ = gen g
      in
        QGen (Q.listOf qg) (shrink gl)
    GenAB ga gb f s ->
      let
        QGen qga _ = gen ga
        QGen qgb _ = gen gb
      in
        QGen (f <$> qga <*> qgb) s
  shrink = \case
    GenBool -> Q.shrink
    GenInt -> Q.shrink
    GenString -> Q.shrink
    GenA _ _ s -> s
    GenAB _ _ _ s -> s
    GenMaybe (g :: Gen TastyTree a) ->
      let
        QGen _ s = gen g
      in
        \case
          -- Matches QuickCheck 2.11.3's implementation. See `Arbitrary1` instance for `Maybe`
          Just a -> Nothing : (Just <$> s a)
          Nothing -> []
    GenList (g :: Gen TastyTree a) ->
      Q.shrinkList (shrink g)

instance PropertyTester TastyTree QGen T.TestName where
  testProperty n = TT . T.testProperty n . T.property

instance T.Testable (Testable TastyTree QGen) where
  property = \case
    B b ->
      T.property b
    Fn foo f ->
      let
        shrink' = shrink foo
        (QGen gen' _) = gen foo
      in
        Q.forAllShrink gen' shrink' f

tastyTest ::
  TastyTree
  -> IO ()
tastyTest = test

