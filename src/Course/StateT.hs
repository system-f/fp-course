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
import qualified Prelude as P

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
instance Functor f => Functor (StateT s f) where
  f <$> StateT k =
    StateT ((<$>) (\(a, t) -> (f a, t)) . k)

-- | Implement the `Apply` instance for @StateT s f@ given a @Applicative f@.
instance Bind f => Apply (StateT s f) where
  StateT f <*> StateT a =
    -- StateT (\s -> (\(g, t) -> (\(z, u) -> (g z, u)) <$> a t) =<< f s)
    StateT ((\(g, t) -> (\(z, u) -> (g z, u)) <$> a t) <=< f)

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
instance Monad f => Applicative (StateT s f) where
  pure a =
    StateT (\s -> pure (a, s))

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
instance Monad f => Bind (StateT s f) where
  f =<< StateT k =
    StateT ((=<<) (\(a, t) -> runStateT (f a) t) . k)

instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values.
state' ::
  (s -> (a, s))
  -> State' s a
state' k =
  StateT (Id . k)

-- | Provide an unwrapper for `State'` values.
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT k) =
  runId . k

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT k) =
  (<$>) snd . k

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' t =
  runId . execT t

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT k) =
  (<$>) fst . k

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' t =
  runId . evalT t

-- | A `StateT` where the state also distributes into the produced value.
getT ::
  Monad f =>
  StateT s f s
getT =
  StateT (\s -> pure (s, s))

-- | A `StateT` where the resulting state is seeded with the given value.
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT =
  StateT . const . pure . (,) ()

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filterM` and `State'` with a @Data.Set#Set@.
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' x =
  eval' (filterM (\a -> state' (\s -> (a `S.notMember` s, a `S.insert` s))) x) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filterM` and `StateT` over `Optional` with a @Data.Set#Set@.
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF x =
  evalT (filterM (\a -> StateT (\s ->
    if a > 100 then Empty else Full (a `S.notMember` s, a `S.insert` s))) x) S.empty

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
instance Functor f => Functor (OptionalT f) where
  f <$> OptionalT x =
    OptionalT ((<$>) f <$> x)

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
instance Apply f => Apply (OptionalT f) where
  OptionalT f <*> OptionalT a =
    OptionalT (lift2 (<*>) f a)

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure =
    OptionalT . pure . pure

-- | Implement the `Bind` instance for `OptionalT f` given a Bind f.
instance Monad f => Bind (OptionalT f) where
  f =<< OptionalT x =
    OptionalT ((\o -> case o of
                        Empty -> pure Empty
                        Full a -> runOptionalT (f a)) =<< x)

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger`.
instance Functor (Logger l) where
  f <$> Logger l a =
    Logger l (f a)

-- | Implement the `Apply` instance for `Logger`.
instance Apply (Logger l) where
  Logger l f <*> Logger m a =
    Logger (l ++ m) (f a)

-- | Implement the `Applicative` instance for `Logger`.
instance Applicative (Logger l) where
  pure =
    Logger Nil

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
instance Bind (Logger l) where
  f =<< Logger l a =
    let Logger l' b = f a
    in Logger (l ++ l') b

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
log1 ::
  l
  -> a
  -> Logger l a
log1 l =
  Logger (l :. Nil)

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
distinctG x =
  runOptionalT (evalT (filterM (\a -> StateT (\s ->
    OptionalT (if a > 100
                 then
                   log1 (fromString ("aborting > 100: " P.++ show a)) Empty
                 else (if even a
                   then log1 (fromString ("even number: " P.++ show a))
                   else pure) (Full (a `S.notMember` s, a `S.insert` s))))) x) S.empty)
