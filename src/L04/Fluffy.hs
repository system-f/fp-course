module L04.Fluffy where

import Prelude hiding (sum, length, map, filter, maximum, reverse)
import L01.Optional
import L01.Validation
import L02.List
import L03.Parser

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy List where
  furry _ Nil    = Nil
  furry f (h:|t) = f h :| furry f t

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
  furry _ Empty = Empty
  furry f (Full a) = Full (f a)


-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
  furry f (P k) = P (\i -> case k i of Error e      -> Error e
                                       Value (r, a) -> Value (r, f a))


-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
