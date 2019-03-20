{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Course.Gens where

import qualified Prelude           as P
import           Test.Mini         (Arbitrary (shrink), Gen (GenList, GenA, GenInt, GenAB))

import           Course.Core
import           Course.List       (List (..), hlist, listh)
import           Course.ListZipper (ListZipper (..))

genList ::
  Arbitrary t g
  => Gen t a
  -> Gen t (List a)
genList g =
  let
    gl = GenList g
  in
    GenA gl listh $ P.fmap listh . shrink gl . hlist

genInteger ::
  forall t g.
  Arbitrary t g
  => Gen t Integer
genInteger =
  let
    toInteger' :: Int -> Integer
    toInteger' = P.fromIntegral

    genInt :: Gen t Int
    genInt = GenInt

    shrink' :: Integer -> [Integer]
    shrink' = P.fmap toInteger' . shrink genInt . P.fromIntegral
  in
    GenA genInt toInteger' shrink'

genIntegerAndList ::
  forall t g.
  Arbitrary t g
  => Gen t (Integer, List Integer)
genIntegerAndList =
  let
    gi :: Gen t Integer
    gi = genInteger

    gl :: Gen t (List Integer)
    gl = genList genInteger
  in
    GenAB gi gl (,) $ \(n, ns) ->
      P.zip (shrink gi n) (shrink gl ns)

genTwoLists ::
  forall t g.
  Arbitrary t g
  => Gen t (List Integer, List Integer)
genTwoLists =
  let
    gl :: Gen t (List Integer)
    gl = genList genInteger
  in
    GenAB gl gl (,) $ \(as, bs) ->
      P.zip (shrink gl as) (shrink gl bs)

genThreeLists ::
  forall t g.
  Arbitrary t g
  => Gen t (List Integer, List Integer, List Integer)
genThreeLists =
  let
    gl :: Gen t (List Integer)
    gl = genList genInteger
  in
    GenAB gl genTwoLists smoosh $ \(a, b, c) ->
      zip3 (shrink gl a) (shrink gl b) (shrink gl c)

genIntegerAndTwoLists ::
  forall t g.
  Arbitrary t g
  => Gen t (Integer, List Integer, List Integer)
genIntegerAndTwoLists =
  let
    sl = shrink (genList genInteger :: Gen t (List Integer))
    si = shrink (genInteger :: Gen t Integer)
  in
    GenAB genInteger genTwoLists smoosh $ \(i, is, is') -> zip3 (si i) (sl is) (sl is')

genListZipper ::
  forall t g.
  Arbitrary t g
  => Gen t (ListZipper Integer)
genListZipper =
  let
    g :: Gen t (Integer, List Integer, List Integer)
    g = genIntegerAndTwoLists

    zippedToZipper (i, is, is') = ListZipper is i is'
    shrinkZipper (ListZipper is i is') = zippedToZipper P.<$> shrink g (i, is, is')
  in
    GenA g zippedToZipper shrinkZipper

genListZipperWithInt ::
  forall t g.
  Arbitrary t g
  => Gen t (ListZipper Integer, Int)
genListZipperWithInt =
  let
    glz :: Gen t (ListZipper Integer)
    glz = genListZipper

    gi :: Gen t Int
    gi = GenInt
  in
    GenAB glz gi (,) $
      \(z, i) -> P.zip (shrink glz z) (shrink gi i)

zip3 ::
  [a]
  -> [b]
  -> [c]
  -> [(a,b,c)]
zip3 as bs =
  P.zipWith smoosh as . P.zip bs

smoosh ::
  a
  -> (b, c)
  -> (a, b, c)
smoosh a (b, c) =
  (a, b, c)
