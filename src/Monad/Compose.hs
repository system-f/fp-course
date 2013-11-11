{-# LANGUAGE NoImplicitPrelude #-}
module Monad.Compose where

import Core
import Monad.Functor
import Monad.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

class Functor f => Applicable f where
  pure ::
    a
    -> f a
  ap ::
    f (a -> b)
    -> f a
    -> f b

newtype Compose f g a =
  Compose (f (g a))

-- Exercise 1
-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  fmap =
    error "todo"

instance (Applicable f, Applicable g) =>
    Applicable (Compose f g) where
-- Exercise 2
-- Implement the pure function for an Applicable instance for Compose
  pure =
    error "todo"
-- Exercise 3
-- Implement the ap function for an Applicable instance for Compose
  ap =
    error "todo"

instance (Monad f, Monad g) =>
    Monad (Compose f g) where
-- Exercise 4
-- Implement the return function for a Monad instance for Compose
  return =
    error "todo"
-- Exercise 5
-- Implement the (>>=) function for a Monad instance for Compose
  bind =
    error "todo"
