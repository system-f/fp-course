module L01.Validation.Arbitrary where

import L01.Validation
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Validation a) where
  arbitrary =
    fmap (either Error Value) arbitrary
