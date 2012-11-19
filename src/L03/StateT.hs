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
  furry f (StateT k) =
      StateT (furry (\(a, t) -> (f a, t)) . k)

instance Misty f => Misty (StateT s f) where
  banana f (StateT k) =
    StateT (banana (\(a, t) -> runStateT (f a) t) . k)
  unicorn a =
    StateT (\s -> unicorn (a, s))

type State' s a =
  StateT s Id a

state' ::
  (s -> (a, s))
  -> State' s a
state' k =
  StateT (Id . k)

runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT k) =
  runId . k

execT ::
  Fluffy f =>
  StateT s f a
  -> s
  -> f s
execT (StateT k) =
  furry snd . k

exec' ::
  State' s a
  -> s
  -> s
exec' t =
  runId . execT t

evalT ::
  Fluffy f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT k) =
  furry fst . k

eval' ::
  State' s a
  -> s
  -> a
eval' t =
  runId . evalT t

getT ::
  Misty f =>
  StateT s f s
getT =
  StateT (\s -> unicorn (s, s))

putT ::
  Misty f =>
  s
  -> StateT s f ()
putT =
  StateT . const . unicorn . (,) ()

distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' x =
  eval' (filterM (\a -> state' (\s -> (a `S.notMember` s, a `S.insert` s))) x) S.empty

distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF x =
  evalT (filterM (\a -> StateT (\s ->
    if a > 100 then Empty else Full (a `S.notMember` s, a `S.insert` s))) x) S.empty

data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

instance Fluffy f => Fluffy (OptionalT f) where
  furry f (OptionalT x) =
    OptionalT (furry (furry f) x)

instance Misty f => Misty (OptionalT f) where
  unicorn =
    OptionalT . unicorn . unicorn
  banana f (OptionalT x) =
    OptionalT (banana (\o -> case o of
                               Empty -> unicorn Empty
                               Full a -> runOptionalT (f a)) x)

distinctG ::
  (Ord a, Num a) =>
  List a
  -> a
  -> Optional (List a)
distinctG x =
  runOptionalT (evalT (filterM (\a -> StateT (\s ->
    OptionalT (\q -> if a > q then Empty else Full (a `S.notMember` s, a `S.insert` s)))) x) S.empty)

data Logger l a =
  Logger [l] a
  deriving (Eq, Show)

instance Fluffy (Logger l) where
  furry f (Logger l a) =
    Logger l (f a)

instance Misty (Logger l) where
  unicorn =
    Logger []
  banana f (Logger l a) =
    let Logger l' b = f a
    in Logger (l ++ l') b

log1 ::
  l
  -> a
  -> Logger l a
log1 l =
  Logger [l]

{-
Remove all duplicate integers from a list. Produce a log as you go. If there is an element above 100, then abort the entire computation and produce no result -- except the log, which contains a String message: "aborting > 100" followed by the value that caused it. If you see an even number, produce a log message, "even number" followed by the even number. Other numbers produce no log message.
-}
distinctH ::
  (Integral a) =>
  List a
  -> Logger String (Optional (List a))
distinctH x =
  runOptionalT (evalT (filterM (\a -> StateT (\s ->
    OptionalT (if a > 100
                 then
                   log1 ("aborting > 100: " ++ show a) Empty
                 else (if even a
                   then log1 ("even number: " ++ show a)
                   else unicorn) (Full (a `S.notMember` s, a `S.insert` s))))) x) S.empty)
