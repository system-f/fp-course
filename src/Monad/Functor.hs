{-# LANGUAGE NoImplicitPrelude #-}

module Monad.Functor where

import Core(IO, Maybe(..), error)
import qualified Prelude as P(fmap)
import Intro.Id(Id(..))
import Intro.Optional(Optional(..))
import Structure.List(List(..))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
--
-- | Maps a function on the Id functor.
--
-- >>> fmap (+1) (Id 2)
-- Id 3
instance Functor Id where
  fmap =
    error "todo"

-- Exercise 2
-- Relative Difficulty: 2
--
-- | Maps a function on the List functor.
--
-- >>> fmap (+1) Nil
-- []
--
-- >>> fmap (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  fmap =
    error "todo"

-- Exercise 3
-- Relative Difficulty: 2
--
-- | Maps a function on the Optional functor.
--
-- >>> fmap (+1) Empty
-- Empty
--
-- >>> fmap (+1) (Full 2)
-- Full 3
instance Functor Optional where
  fmap =
    error "todo"

-- Exercise 4
-- Relative Difficulty: 3
--
-- | Maps a function on the reader ((->) t) functor.
--
-- >>> fmap (+1) (*2) 8
-- 17
instance Functor ((->) t) where
  fmap =
    error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> fmap reverse (putStr "hi" >> return "abc")
-- hi"cba"
instance Functor IO where
  fmap =
    P.fmap

instance Functor [] where
  fmap =
    P.fmap

instance Functor Maybe where
  fmap =
    P.fmap
