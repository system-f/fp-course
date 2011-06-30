module L10.Compose where

import Control.Applicative

-- Exactly one of these exercises will not be possible to achieve.

newtype Compose f g a =
  Compose (f (g a))

-- Exercise 1
-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  fmap f (Compose k) =
    error "todo"

instance (Applicative f, Applicative g) =>
Applicative (Compose f g) where
-- Exercise 2
-- Implement the pure function for an Applicative instance for Compose
  pure = 
    error "todo"
-- Exercise 3
-- Implement the (<*>) function for an Applicative instance for Compose
  Compose f <*> Compose a =
    error "todo"

instance (Monad f, Monad g) =>
    Monad (Compose f g) where
-- Exercise 4
-- Implement the return function for a Monad instance for Compose
  return = 
    error "todo"
-- Exercise 5
-- Implement the (>>=) function for a Monad instance for Compose
  Compose a >>= f =
    error "todo"

