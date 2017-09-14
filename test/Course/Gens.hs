{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Gens where

import           Control.Applicative (liftA2, liftA3)
import qualified Prelude             as P (fmap, foldr)
import           Test.QuickCheck     (Arbitrary (..), Gen, Property, Testable,
                                      forAllShrink)

import           Course.Core
import           Course.List         (List (..), hlist, listh)

genList :: Arbitrary a => Gen (List a)
genList = P.fmap ((P.foldr (:.) Nil) :: [a] -> List a) arbitrary

shrinkList :: Arbitrary a => List a -> [List a]
shrinkList =
  P.fmap listh . shrink . hlist

genIntegerList :: Gen (List Integer)
genIntegerList = genList

genIntegerAndList :: Gen (Integer, List Integer)
genIntegerAndList = P.fmap (P.fmap listh) arbitrary

shrinkIntegerAndList :: (Integer, List Integer) -> [(Integer, List Integer)]
shrinkIntegerAndList = P.fmap (P.fmap listh) . shrink . P.fmap hlist

genTwoLists :: Gen (List Integer, List Integer)
genTwoLists = liftA2 (,) genIntegerList genIntegerList -- (arbitrary :: (List Integer, List Integer))

shrinkTwoLists :: (List Integer, List Integer) -> [(List Integer, List Integer)]
shrinkTwoLists (a,b) = P.fmap (\(as,bs) -> (listh as, listh bs)) $ shrink (hlist a, hlist b)

genThreeLists :: Gen (List Integer, List Integer, List Integer)
genThreeLists = liftA3 (,,) genIntegerList genIntegerList genIntegerList

shrinkThreeLists :: (List Integer, List Integer, List Integer) -> [(List Integer, List Integer, List Integer)]
shrinkThreeLists (a,b,c) = P.fmap (\(as,bs,cs) -> (listh as, listh bs, listh cs)) $ shrink (hlist a, hlist b, hlist c)

genListOfLists :: Gen (List (List Integer))
genListOfLists = P.fmap (P.fmap listh) (genList :: (Gen (List [Integer])))

shrinkListOfLists :: Arbitrary a => List (List a) -> [List (List a)]
shrinkListOfLists = P.fmap (P.fmap listh). shrinkList . P.fmap hlist

forAllLists :: Testable prop => (List Integer -> prop) -> Property
forAllLists = forAllShrink genIntegerList shrinkList
