module Structure.MetricSpace where

import Algorithm.EditDistance

class Eq a => MetricSpace a where
  (<-->) ::
    a
    -> a
    -> Int

instance Eq a => MetricSpace [a] where
  (<-->) =
    editDistance

