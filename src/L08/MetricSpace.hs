module L08.MetricSpace where

import L08.EditDistance

class Eq a => MetricSpace a where
  (<-->) ::
    a
    -> a
    -> Int

instance Eq a => MetricSpace [a] where
  (<-->) =
    editDistance

