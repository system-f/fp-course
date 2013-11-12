module Course.Applicative where

import Course.Apply

class Apply f => Applicative f where
  pure ::
    a -> f a

  (.<$>.) ::
    (a -> b)
    -> f a
    -> f b
