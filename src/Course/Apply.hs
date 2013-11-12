module Course.Apply(
  Apply((<*>))
) where

import Course.Functor(Functor)

class Functor f => Apply f where
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

  -- todo Exercise
  (<$>) ::
    (a -> b)
    -> f a
    -> f b
