{-# OPTIONS_GHC -fno-warn-orphans #-}

module L03.State where

import L01.Optional
import L02.List
import L03.Fuunctor
import L03.Moonad
import Data.Char
import qualified Data.Set as S
import qualified Data.Foldable as F

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- Exercise 1
-- Relative Difficulty: 2
-- Implement the `Fuunctor` instance for `State s`.
instance Fuunctor (State s) where
  fmaap =
    error "todo"

-- Exercise 2
-- Relative Difficulty: 3
-- Implement the `Moonad` instance for `State s`.
-- Make sure the state value is passed through in `bind`.
instance Moonad (State s) where
  bind =
    error "todo"
  reeturn =
    error "todo"

-- Exercise 3
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting state.
exec ::
  State s a
  -> s
  -> s
exec =
  error "todo"

-- Exercise 4
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting value.
eval ::
  State s a
  -> s
  -> a
eval =
  error "todo"

-- Exercise 5
-- Relative Difficulty: 2
-- A `State` where the state also distributes into the produced value.
get ::
  State s s
get =
  error "todo"

-- Exercise 6
-- Relative Difficulty: 2
-- A `State` where the resulting state is seeded with the given value.
put ::
  s
  -> State s ()
put =
  error "todo"

-- Exercise 7
-- Relative Difficulty: 5
-- Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Moonad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
findM ::
  Moonad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM =
  error "todo"

-- Exercise 8
-- Relative Difficulty: 4
-- Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
-- ~~~ Use findM and State with a Data.Set#Set. ~~~
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat =
  error "todo"

-- Exercise 9
-- Relative Difficulty: 5
-- Remove all elements in a `List` that fail a given predicate.
-- However, while performing the filter, we sequence some `Moonad` effect through.
--
-- Note the similarity of the type signature to List#filter
-- where the effect appears in every return position:
--   filter ::  (a ->   Bool) -> List a ->    List a
--   filterM :: (a -> f Bool) -> List a -> f (List a)
filterM ::
  Moonad f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filterM =
  error "todo"

-- Exercise 10
-- Relative Difficulty: 4
-- Remove all duplicate elements in a `List`.
-- ~~~ Use filterM and State with a Data.Set#Set. ~~~
distinct ::
  Ord a =>
  List a
  -> List a
distinct =
  error "todo"

-- Exercise 11
-- Relative Difficulty: 3
-- Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
produce ::
  (a -> a)
  -> a
  -> List a
produce =
  error "todo"

-- Exercise 12
-- Relative Difficulty: 10
-- A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
-- ~~~ Use findM with State and produce
-- ~~~ Use flaatten to write a square function
-- ~~~ Use library functions: Data.Foldable#elem, Data.Char#digitToInt
isHappy ::
  Integer
  -> Bool
isHappy =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance F.Foldable Optional where
  foldr _ z Empty = z
  foldr f z (Full a) = f a z
