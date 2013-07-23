module L04.ListZipper.Arbitrary where

import L04.ListZipper
import Test.QuickCheck

instance Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary =
    do l <- arbitrary
       x <- arbitrary
       r <- arbitrary
       return (ListZipper l x r)