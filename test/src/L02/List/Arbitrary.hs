module L02.List.Arbitrary where

import L02.List
import Test.QuickCheck

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    fmap (foldr (:.) Nil) arbitrary