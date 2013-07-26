module Intro.ArbitraryValidation where

import Intro.Validation
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Validation a) where
  arbitrary =
    fmap (either Error Value) arbitrary
