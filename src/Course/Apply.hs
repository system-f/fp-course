{-# LANGUAGE NoImplicitPrelude #-}

module Course.Apply where

import Course.Core
import Course.Functor
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class Functor f => Apply f where
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

infixl 4 <*>

instance Apply Id where
  (<*>) =
    error "todo"

instance Apply List where
  (<*>) =
    error "todo"

instance Apply Optional where
  (<*>) =
    error "todo"

instance Apply ((->) t) where
  (<*>) =
    error "todo"

-- Exercise 13
--
-- | Apply a binary function in the environment.
--
-- >>> lift2 (+) (Id 7) (Id 8)
-- Id 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) len sum (listh [4,5,6])
-- 18
lift2 ::
  Apply f =>
  (a -> b -> c)
  -> f a
  -> f b
  -> f c
lift2 =
  error "todo"

-- Exercise 14
-- (bonus: use apply + lift2)
--
-- | Apply a ternary function in the Monad environment.
--
-- >>> lift3 (\a b c -> a + b + c) (Id 7) (Id 8) (Id 9)
-- Id 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) len sum product (listh [4,5,6])
-- 138
lift3 ::
  Apply f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
lift3 =
  error "todo"

-- Exercise 15
--
-- | Apply a quaternary function in the environment.
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Id 7) (Id 8) (Id 9) (Id 10)
-- Id 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) len sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Apply f =>
  (a -> b -> c -> d -> e)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
lift4 =
  error "todo"

(*>) ::
  Apply f =>
  f a
  -> f b
  -> f b
(*>) =
  error "todo"

(<*) ::
  Apply f =>
  f b
  -> f a
  -> f b
(<*) =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Apply IO where
  f <*> a =
    f P.>>= \f' -> P.fmap (f' $) a

instance Apply [] where
  f <*> a =
    f P.>>= \f' -> P.fmap (f' $) a

instance Apply P.Maybe where
  f <*> a =
    f P.>>= \f' -> P.fmap (f' $) a
