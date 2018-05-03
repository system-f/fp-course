{-# LANGUAGE NoImplicitPrelude #-}

module Course.MonadTutorial where

import Control.Category(Category((.)))
import Control.Monad(Monad(..), (=<<))
import Data.Eq(Eq)
import Data.Foldable(foldr)
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.String(IsString(fromString))
import Prelude(Show)
import System.IO(IO)

{-

--------------------------------------------------------------------------------
WARNING: DO NOT PROCEED
-----------------------

It is strongly advised that pre-requisite exercises have been covered prior to
utilising this tutorial. Refusing this advice increases likelihood of a crash
and burn result.

Please complete the following exercises before proceeding:
* Course/Functor
* Course/Applicative
--------------------------------------------------------------------------------

In this source file, you will find a recurring pattern:

* A data structure definition.
* A function named @bind<name>@ for that data structure. The bind function will
  follow a specific pattern in its type:

  @(a -> f b) -> f a -> f b@

* A function named @pure<name>@ for that data structure. The pure function will
  follow a specific pattern in its type:

  @a -> f a@

* A function named @sequence<name>@ for that data structure. The sequence
  function will follow a specific pattern in its type:

  @[f a] -> f [a]

Note that the sequence functions are written in terms of the bind and pure
functions for that data type. The goal is to first acknowledge the repeating
code in the sequence functions, and then construct a plan to refactor out the
similarities. Ultimately, there should be only a single sequence function that is
written in terms of "things that have bind and pure functions."

A type-class denoting "things that have bind and pure functions" is provided. It
is named @BindAndPure@.

Examine the existing data structures, their implementations of bind and pure,
then implement a single sequence function that generalises all the specific
sequence functions.

The data structures given are:
* Id
* Optional
* IntReader
* Reader
* IntState
* State
* Or
* ListFree
* IntReaderFree
* ReaderFree
* Free
* IO

-}

data Id a =
  Id a
  deriving (Eq, Show)

bindId ::
  (a -> Id b)
  -> Id a
  -> Id b
bindId f (Id a) =
  f a

pureId ::
  a
  -> Id a
pureId =
  Id

sequenceId ::
  [Id a]
  -> Id [a]
sequenceId =
  foldr (\a as ->
    bindId (\a' ->
    bindId (\as' ->
    pureId (a' : as')) as) a)
  (pureId [])

----

data Optional a =
  Empty
  | Full a
  deriving (Eq, Show)

bindOptional ::
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional _ Empty =
  Empty
bindOptional f (Full a) =
  f a

pureOptional ::
  a
  -> Optional a
pureOptional =
  Full

sequenceOptional ::
  [Optional a]
  -> Optional [a]
sequenceOptional =
  foldr (\a as ->
    bindOptional (\a' ->
    bindOptional (\as' ->
    pureOptional (a' : as')) as) a)
  (pureOptional [])

----

data IntReader a =
  IntReader (Int -> a)

bindIntReader ::
  (a -> IntReader b)
  -> IntReader a
  -> IntReader b
bindIntReader f (IntReader g) =
  IntReader (\x -> let IntReader r = f (g x) in r x)

pureIntReader ::
  a
  -> IntReader a
pureIntReader =
  IntReader . return

sequenceIntReader ::
  [IntReader a]
  -> IntReader [a]
sequenceIntReader =
  foldr (\a as ->
    bindIntReader (\a' ->
    bindIntReader (\as' ->
    pureIntReader (a' : as')) as) a)
  (pureIntReader [])

----

data Reader r a =
  Reader (r -> a)
  
bindReader ::
  (a -> Reader r b)
  -> Reader r a
  -> Reader r b
bindReader f (Reader g) =
  Reader (\x -> let Reader r = f (g x) in r x)

pureReader ::
  a
  -> Reader r a
pureReader =
  Reader . return

sequenceReader ::
  [Reader r a]
  -> Reader r [a]
sequenceReader =
  foldr (\a as ->
    bindReader (\a' ->
    bindReader (\as' ->
    pureReader (a' : as')) as) a)
  (pureReader [])

----

data IntState a =
  IntState (Int -> (a, Int))
  
bindIntState ::
  (a -> IntState b)
  -> IntState a
  -> IntState b
bindIntState f (IntState g) =
  IntState (\i -> 
    let (a, j) = g i
        IntState h = f a
    in h j)

pureIntState ::
  a
  -> IntState a
pureIntState a =
  IntState (\i -> (a, i))

sequenceIntState ::
  [IntState a]
  -> IntState [a]
sequenceIntState =
  foldr (\a as ->
    bindIntState (\a' ->
    bindIntState (\as' ->
    pureIntState (a' : as')) as) a)
  (pureIntState [])

----

data State s a =
  State (s -> (a, s))

bindState ::
  (a -> State s b)
  -> State s a
  -> State s b
bindState f (State g) =
  State (\s -> 
    let (a, t) = g s
        State h = f a
    in h t)

pureState ::
  a
  -> State s a
pureState a =
  State (\s -> (a, s))

sequenceState ::
  [State s a]
  -> State s [a]
sequenceState =
  foldr (\a as ->
    bindState (\a' ->
    bindState (\as' ->
    pureState (a' : as')) as) a)
  (pureState [])

----

data Or t a =
  This t
  | That a
  deriving (Eq, Show)

bindOr ::
  (a -> Or t b)
  -> Or t a
  -> Or t b
bindOr _ (This t) =
  This t
bindOr f (That a) =
  f a

pureOr ::
  a
  -> Or t a
pureOr =
  That

sequenceOr ::
  [Or t a]
  -> Or t [a]
sequenceOr =
  foldr (\a as ->
    bindOr (\a' ->
    bindOr (\as' ->
    pureOr (a' : as')) as) a)
  (pureOr [])

----

data ListFree a =
  ListDone a
  | ListMore [ListFree a]
  deriving (Eq, Show)

bindListFree ::
  (a -> ListFree b)
  -> ListFree a
  -> ListFree b
bindListFree f (ListDone a) =
  f a
bindListFree f (ListMore r) =
  ListMore (fmap (bindListFree f) r)

pureListFree ::
  a
  -> ListFree a
pureListFree =
  ListDone

sequenceListFree ::
  [ListFree a]
  -> ListFree [a]
sequenceListFree =
  foldr (\a as ->
    bindListFree (\a' ->
    bindListFree (\as' ->
    pureListFree (a' : as')) as) a)
  (pureListFree [])

----

data IntReaderFree a =
  IntReaderDone a
  | IntReaderMore [IntReaderFree a]
  deriving (Eq, Show)

bindIntReaderFree ::
  (a -> IntReaderFree b)
  -> IntReaderFree a
  -> IntReaderFree b
bindIntReaderFree f (IntReaderDone a) =
  f a
bindIntReaderFree f (IntReaderMore r) =
  IntReaderMore (fmap (bindIntReaderFree f) r)

pureIntReaderFree ::
  a
  -> IntReaderFree a
pureIntReaderFree =
  IntReaderDone

sequenceIntReaderFree ::
  [IntReaderFree a]
  -> IntReaderFree [a]
sequenceIntReaderFree =
  foldr (\a as ->
    bindIntReaderFree (\a' ->
    bindIntReaderFree (\as' ->
    pureIntReaderFree (a' : as')) as) a)
  (pureIntReaderFree [])

----

data ReaderFree r a =
  ReaderDone a
  | ReaderMore (Reader r (ReaderFree r a))

bindReaderFree ::
  (a -> ReaderFree r b)
  -> ReaderFree r a
  -> ReaderFree r b
bindReaderFree f (ReaderDone a) =
  f a
bindReaderFree f (ReaderMore (Reader r)) =
  ReaderMore (Reader (bindReaderFree f . r))

pureReaderFree ::
  a
  -> ReaderFree r a
pureReaderFree =
  ReaderDone

sequenceReaderFree ::
  [ReaderFree r a]
  -> ReaderFree r [a]
sequenceReaderFree =
  foldr (\a as ->
    bindReaderFree (\a' ->
    bindReaderFree (\as' ->
    pureReaderFree (a' : as')) as) a)
  (pureReaderFree [])

----

data Free f a =
  Done a
  | More (f (Free f a))

bindFree ::
  Functor f =>
  (a -> Free f b)
  -> Free f a
  -> Free f b
bindFree f (Done a) =
  f a
bindFree f (More r) =
  More (fmap (bindFree f) r)

pureFree ::
  a
  -> Free f a
pureFree =
  Done

sequenceFree ::
  Functor f =>
  [Free f a]
  -> Free f [a]
sequenceFree =
  foldr (\a as ->
    bindFree (\a' ->
    bindFree (\as' ->
    pureFree (a' : as')) as) a)
  (pureFree [])

----

-- data IO = …

bindIO ::
  (a -> IO b)
  -> IO a
  -> IO b
bindIO f o =
  f =<< o

pureIO ::
  a
  -> IO a
pureIO =
  return

sequenceIO ::
  [IO a]
  -> IO [a]
sequenceIO =
  foldr (\a as ->
    bindIO (\a' ->
    bindIO (\as' ->
    pureIO (a' : as')) as) a)
  (pureIO [])

----

class BindAndPure f where
  bind ::
    (a -> f b)
    -> f a
    -> f b
  pure ::
    a
    -> f a
    
