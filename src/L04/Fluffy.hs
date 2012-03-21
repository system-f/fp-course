module L04.Fluffy where

import L01.Optional
import L01.Validation
import L02.List
import L03.Parser

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy List where
  furry =
    error "todo"

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
  furry =
    error "todo"


-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
  furry =
    error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
