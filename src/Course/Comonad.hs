module Course.Comonad where

import Course.Extend
import Course.Id

class Extend f => Comonad f where
  copure ::
    f a
    -> a

instance Comonad Id where
  copure (Id a) =
    a
