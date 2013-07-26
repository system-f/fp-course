module Structure.ArbitraryListZipper where

import Structure.ListZipper
import Test.QuickCheck

instance Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary =
    do l <- arbitrary
       x <- arbitrary
       r <- arbitrary
       return (ListZipper l x r)