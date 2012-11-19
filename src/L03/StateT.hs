module L03.StateT where

import L01.Id
import L01.Optional
import L02.List
import L03.Fluffy
import L03.Misty
import L03.State
import qualified Data.Set as S
import qualified Data.Foldable as F

newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

instance Fluffy f => Fluffy (StateT s f) where
  furry =
    error "todo"

instance Misty f => Misty (StateT s f) where
  banana =
    error "todo"
  unicorn =
    error "todo"

type State' s a =
  StateT s Id a

state' ::
  (s -> (a, s))
  -> State' s a
state' =
  error "todo"

runState' ::
  State' s a
  -> s
  -> (a, s)
runState' =
  error "todo"

execT ::
  Fluffy f =>
  StateT s f a
  -> s
  -> f s
execT =
  error "todo"

exec' ::
  State' s a
  -> s
  -> s
exec' =
  error "todo"

evalT ::
  Fluffy f =>
  StateT s f a
  -> s
  -> f a
evalT =
  error "todo"

eval' ::
  State' s a
  -> s
  -> a
eval' =
  error "todo"

getT ::
  Misty f =>
  StateT s f s
getT =
  error "todo"

putT ::
  Misty f =>
  s
  -> StateT s f ()
putT =
  error "todo"

distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' =
  error "todo"

distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF =
  error "todo"

data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

instance Fluffy f => Fluffy (OptionalT f) where
  furry =
    error "todo"

instance Misty f => Misty (OptionalT f) where
  unicorn =
    error "todo"
  banana =
    error "todo"

distinctG ::
  (Ord a, Num a) =>
  List a
  -> a
  -> Optional (List a)
distinctG =
  error "todo"

data Logger l a =
  Logger [l] a
  deriving (Eq, Show)

instance Fluffy (Logger l) where
  furry =
    error "todo"

instance Misty (Logger l) where
  unicorn =
    error "todo"
  banana =
    error "todo"

log1 ::
  l
  -> a
  -> Logger l a
log1 =
  error "todo"

{-
Remove all duplicate integers from a list. Produce a log as you go. If there is an element above 100, then abort the entire computation and produce no result -- except the log, which contains a String message: "aborting > 100" followed by the value that caused it. If you see an even number, produce a log message, "even number" followed by the even number. Other numbers produce no log message.
-}
distinctH ::
  (Integral a) =>
  List a
  -> Logger String (Optional (List a))
distinctH =
  error "todo"
