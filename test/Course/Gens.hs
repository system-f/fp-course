{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Course.Gens where

import qualified Prelude             as P --(fmap, foldr, (<$>), (<*>))
import Test.Mini (PropertyTester (..), Arbitrary (..), Gen (..), Testable)

import           Course.Core
import           Course.List         (List (..), hlist, listh)
import           Course.ListZipper   (ListZipper (..), zipper)

genList ::
  Arbitrary t g
  => Gen t [a]
  -> Gen t (List a)
genList gl =
  GenA gl listh $ P.fmap listh . shrink gl . hlist

genInteger ::
  Arbitrary t g
  => Gen t Integer
genInteger =
  let
    toInteger = P.fromIntegral :: Int -> Integer
  in
    GenA GenInt toInteger (P.fmap toInteger . shrink GenInt)

genIntegerList :: Gen t (List Integer)
genIntegerList = genList $ GenA GenInt

genIntegerAndList :: Gen t (Integer, List Integer)
genIntegerAndList =
  GenAB genInteger genIntegerList (,) $ \(n, ns) ->
    P.zip (shrink genInteger n) (shrink genIntegerList)

genTwoLists :: Gen t (List Integer, List Integer)
genTwoLists = GenAB genIntegerList genIntegerList (,) $ \(as, bs) ->
  P.zip (shrink genIntegerList as) (shrink genIntegerList bs)

-- genThreeLists :: Gen t (List Integer, List Integer, List Integer)
-- genThreeLists = (,,) P.<$> genIntegerList P.<*> genIntegerList P.<*> genIntegerList

-- shrinkThreeLists :: (List Integer, List Integer, List Integer) -> [(List Integer, List Integer, List Integer)]
-- shrinkThreeLists (a,b,c) = P.fmap (\(as,bs,cs) -> (listh as, listh bs, listh cs)) $ shrink (hlist a, hlist b, hlist c)

-- genListOfLists :: gen (List (List Integer))
-- genListOfLists = P.fmap (P.fmap listh) (genList :: (gen (List [Integer])))

-- --shrinkListOfLists :: Arbitrary a => List (List a) -> [List (List a)]
-- shrinkListOfLists :: List (List a) -> [List (List a)]
-- shrinkListOfLists = P.fmap (P.fmap listh). shrinkList . P.fmap hlist

-- forAllLists :: (List Integer -> prop) -> Property t
-- forAllLists = forAllShrink genIntegerList shrinkList

-- -- (List Integer) and a Bool
-- genListAndBool :: gen (List Integer, Bool)
-- genListAndBool = (,) P.<$> genIntegerList P.<*> arbitrary

-- shrinkListAndBool :: (List Integer, Bool) -> [(List Integer, Bool)]
-- shrinkListAndBool (xs,b) = (,) P.<$> (shrinkList xs) P.<*> (shrink b)

-- forAllListsAndBool ::
--   ((List Integer, Bool) -> prop)
--   -> Property t
-- forAllListsAndBool =
--   forAllShrink genListAndBool shrinkListAndBool

-- -- ListZipper Integer
-- genListZipper :: gen (ListZipper Integer)
-- genListZipper =
--   zipper P.<$> arbitrary P.<*> arbitrary P.<*> arbitrary

-- shrinkListZipper :: ListZipper Integer -> [ListZipper Integer]
-- shrinkListZipper (ListZipper l x r) =
--   ListZipper P.<$> (shrinkList l) P.<*> (shrink (undefined :: t) x) P.<*> (shrinkList r)

-- forAllListZipper ::
--   (ListZipper Integer -> prop)
--   -> Property t
-- forAllListZipper =
--   forAllShrink genListZipper shrinkListZipper

-- genListZipperWithInt :: gen (ListZipper Integer, Int)
-- genListZipperWithInt =
--   (,) P.<$> genListZipper P.<*> arbitrary

-- shrinkListZipperWithInt :: (ListZipper Integer, Int) -> [(ListZipper Integer, Int)]
-- shrinkListZipperWithInt (z, i) =
--   (,) P.<$> (shrinkListZipper z) P.<*> (shrink i)

-- forAllListZipperWithInt ::
--   ((ListZipper Integer, Int) -> prop)
--   -> Property t
-- forAllListZipperWithInt =
--   forAllShrink genListZipperWithInt shrinkListZipperWithInt
