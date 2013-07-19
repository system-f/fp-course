module L03.Fuunctor where

import L01.Id
import L01.Optional
import L01.Validation
import L02.List

class Fuunctor f where
  fmaap :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
--
-- | Maps a function on the Id functor.
--
-- Examples:
--
-- >>> fmaap (+1) (Id 2)
-- Id 3
instance Fuunctor Id where
  fmaap =
    error "todo"

-- Exercise 2
-- Relative Difficulty: 2
--
-- | Maps a function on the List functor.
--
-- Examples:
--
-- >>> fmaap (+1) Nil
-- []
--
-- >>> fmaap (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Fuunctor List where
  fmaap =
    error "todo"

-- Exercise 3
-- Relative Difficulty: 2
--
-- | Maps a function on the Optional functor.
--
-- Examples:
--
-- >>> fmaap (+1) Empty
-- Empty
--
-- >>> fmaap (+1) (Full 2)
-- Full 3
instance Fuunctor Optional where
  fmaap =
    error "todo"

-- Exercise 4
-- Relative Difficulty: 3
--
-- | Maps a function on the reader ((->) t) functor.
--
-- Examples:
--
-- >>> fmaap (+1) (*2) 8
-- 17
instance Fuunctor ((->) t) where
  fmaap =
    error "todo"

-- Exercise 5
-- Relative Difficulty: 2
instance Fuunctor IO where
  fmaap =
    error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fuunctor [] where
  fmaap = fmap
