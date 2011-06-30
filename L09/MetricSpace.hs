module L09.MetricSpace where

import L09.EditDistance

class Eq a => MetricSpace a where
  (<-->) ::
    a
    -> a
    -> Int

instance Eq a => MetricSpace [a] where
  (<-->) =
    editDistance

