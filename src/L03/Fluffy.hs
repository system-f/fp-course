module L03.Fluffy where

import L01.Id
import L01.Optional
import L01.Validation
import L02.List

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy Id where
  furry =
    mapId

-- Exercise 2
-- Relative Difficulty: 2
instance Fluffy List where
  furry =
    maap

-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Optional where
  furry =
    mapOptional

-- Exercise 4
-- Relative Difficulty: 3
instance Fluffy ((->) t) where
  furry f g =
    \x -> f (g x)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
