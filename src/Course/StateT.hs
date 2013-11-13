{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.State
import qualified Data.Set as S

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- Exercise 1
-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
instance Functor f => Functor (StateT s f) where
  (<$>) =
    error "todo"

-- | Implement the `Apply` instance for @StateT s f@ given a @Applicative f@.
instance Bind f => Apply (StateT s f) where
  (<*>) =
    error "todo"

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
instance Monad f => Applicative (StateT s f) where
  pure =
    error "todo"

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
instance Monad f => Bind (StateT s f) where
  (=<<) =
    error "todo"

instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- Exercise 3
-- | Provide a constructor for `State'` values.
state' ::
  (s -> (a, s))
  -> State' s a
state' =
  error "todo"

-- Exercise 4
--
-- | Provide an unwrapper for `State'` values.
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' =
  error "todo"

-- Exercise 5
-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT =
  error "todo"

-- Exercise 6
-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' =
  error "todo"

-- Exercise 7
-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT =
  error "todo"

-- Exercise 8
-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' =
  error "todo"

-- Exercise 9
-- | A `StateT` where the state also distributes into the produced value.
getT ::
  Monad f =>
  StateT s f s
getT =
  error "todo"

-- Exercise 10
-- | A `StateT` where the resulting state is seeded with the given value.
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT =
  error "todo"

-- Exercise 11
-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filterM` and `State'` with a @Data.Set#Set@.
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' =
  error "todo"

-- Exercise 12
-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filterM` and `StateT` over `Optional` with a @Data.Set#Set@.
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF =
  error "todo"

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- Exercise 13
-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
instance Functor f => Functor (OptionalT f) where
  (<$>) =
    error "todo"

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
instance Apply f => Apply (OptionalT f) where
  (<*>) =
    error "todo"

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure =
    error "todo"

-- | Implement the `Bind` instance for `OptionalT f` given a Bind f.
instance Bind f => Bind (OptionalT f) where
  (=<<) =
    error "todo"

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger [l] a
  deriving (Eq, Show)

-- Exercise 15
-- | Implement the `Functor` instance for `Logger`.
instance Functor (Logger l) where
  (<$>) =
    error "todo"

-- | Implement the `Apply` instance for `Logger`.
instance Apply (Logger l) where
  (<*>) =
    error "todo"

-- | Implement the `Applicative` instance for `Logger`.
instance Applicative (Logger l) where
  pure =
    error "todo"

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
instance Bind (Logger l) where
  (=<<) =
    error "todo"

instance Monad (Logger l) where

-- Exercise 17
-- | A utility function for producing a `Logger` with one log value.
log1 ::
  l
  -> a
  -> Logger l a
log1 =
  error "todo"

-- Exercise 18
-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filterM` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Str (Optional (List a))
distinctG =
  error "todo"
