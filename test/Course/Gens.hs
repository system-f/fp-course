{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Course.Gens where

import qualified Prelude             as P --(fmap, foldr, (<$>), (<*>))
-- import           Test.QuickCheck     (Arbitrary (..), Gen, Property, Testable,
--                                       forAllShrink)
import GHC.Exts (Constraint)
import Test.Mini (PropertyTester (..))

import           Course.Core
import           Course.List         (List (..), hlist, listh)
import           Course.ListZipper   (ListZipper (..), zipper)

type G a = forall t name. PropertyTester t name => Gen t a

--type family A'' a t name :: Constraint
type A t name a = (PropertyTester t name, Arbitrary t a)

-- type family A' a :: Constraint
-- type instance A' a = forall t name. A'' a t name

--type A a = forall t name. A'' a t name --(PropertyTester t name, Arbitrary t a)

--genList :: Arbitrary a => Gen (List a)
genList :: G (List a)
genList = P.fmap (P.foldr (:.) Nil :: [a] -> List a) arbitrary

--shrinkList :: Arbitrary a => List a -> [List a]
shrinkList :: forall a t name. A t name a => List a -> [List a]
shrinkList =
  P.fmap listh . shrink . hlist

genIntegerList :: gen (List Integer)
genIntegerList = genList

genIntegerAndList :: gen (Integer, List Integer)
genIntegerAndList = P.fmap (P.fmap listh) arbitrary

shrinkIntegerAndList :: (Integer, List Integer) -> [(Integer, List Integer)]
shrinkIntegerAndList = P.fmap (P.fmap listh) . shrink . P.fmap hlist

genTwoLists :: gen (List Integer, List Integer)
genTwoLists = (,) P.<$> genIntegerList P.<*> genIntegerList

shrinkTwoLists :: (List Integer, List Integer) -> [(List Integer, List Integer)]
shrinkTwoLists (a,b) = P.fmap (\(as,bs) -> (listh as, listh bs)) $ shrink (hlist a, hlist b)

genThreeLists :: gen (List Integer, List Integer, List Integer)
genThreeLists = (,,) P.<$> genIntegerList P.<*> genIntegerList P.<*> genIntegerList

shrinkThreeLists :: (List Integer, List Integer, List Integer) -> [(List Integer, List Integer, List Integer)]
shrinkThreeLists (a,b,c) = P.fmap (\(as,bs,cs) -> (listh as, listh bs, listh cs)) $ shrink (hlist a, hlist b, hlist c)

genListOfLists :: gen (List (List Integer))
genListOfLists = P.fmap (P.fmap listh) (genList :: (gen (List [Integer])))

--shrinkListOfLists :: Arbitrary a => List (List a) -> [List (List a)]
shrinkListOfLists :: List (List a) -> [List (List a)]
shrinkListOfLists = P.fmap (P.fmap listh). shrinkList . P.fmap hlist

forAllLists :: (List Integer -> prop) -> Property t
forAllLists = forAllShrink genIntegerList shrinkList

-- (List Integer) and a Bool
genListAndBool :: gen (List Integer, Bool)
genListAndBool = (,) P.<$> genIntegerList P.<*> arbitrary

shrinkListAndBool :: (List Integer, Bool) -> [(List Integer, Bool)]
shrinkListAndBool (xs,b) = (,) P.<$> (shrinkList xs) P.<*> (shrink b)

forAllListsAndBool ::
  ((List Integer, Bool) -> prop)
  -> Property t
forAllListsAndBool =
  forAllShrink genListAndBool shrinkListAndBool

-- ListZipper Integer
genListZipper :: gen (ListZipper Integer)
genListZipper =
  zipper P.<$> arbitrary P.<*> arbitrary P.<*> arbitrary

shrinkListZipper :: ListZipper Integer -> [ListZipper Integer]
shrinkListZipper (ListZipper l x r) =
  ListZipper P.<$> (shrinkList l) P.<*> (shrink (undefined :: t) x) P.<*> (shrinkList r)

forAllListZipper ::
  (ListZipper Integer -> prop)
  -> Property t
forAllListZipper =
  forAllShrink genListZipper shrinkListZipper

genListZipperWithInt :: gen (ListZipper Integer, Int)
genListZipperWithInt =
  (,) P.<$> genListZipper P.<*> arbitrary

shrinkListZipperWithInt :: (ListZipper Integer, Int) -> [(ListZipper Integer, Int)]
shrinkListZipperWithInt (z, i) =
  (,) P.<$> (shrinkListZipper z) P.<*> (shrink i)

forAllListZipperWithInt ::
  ((ListZipper Integer, Int) -> prop)
  -> Property t
forAllListZipperWithInt =
  forAllShrink genListZipperWithInt shrinkListZipperWithInt
