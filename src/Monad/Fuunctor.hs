module Monad.Fuunctor where

import Intro.Id
import Intro.Optional
import Intro.Validation
import Structure.List

class Fuunctor f where
  fmaap :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
--
-- | Maps a function on the Id functor.
--
-- >>> fmaap (+1) (Id 2)
-- Id 3
instance Fuunctor Id where
  fmaap =
    mapId

-- Exercise 2
-- Relative Difficulty: 2
--
-- | Maps a function on the List functor.
--
-- >>> fmaap (+1) Nil
-- []
--
-- >>> fmaap (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Fuunctor List where
  fmaap =
    maap

-- Exercise 3
-- Relative Difficulty: 2
--
-- | Maps a function on the Optional functor.
--
-- >>> fmaap (+1) Empty
-- Empty
--
-- >>> fmaap (+1) (Full 2)
-- Full 3
instance Fuunctor Optional where
  fmaap =
    mapOptional

-- Exercise 4
-- Relative Difficulty: 3
--
-- | Maps a function on the reader ((->) t) functor.
--
-- >>> fmaap (+1) (*2) 8
-- 17
instance Fuunctor ((->) t) where
  fmaap f g =
    \x -> f (g x)

-- Exercise 5
-- Relative Difficulty: 2
--
-- | Maps a function on an IO program.
--
-- >>> fmaap reverse (putStr "hi" >> return "abc")
-- hi"cba"
instance Fuunctor IO where
  fmaap =
    fmap

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fuunctor [] where
  fmaap =
    fmap
