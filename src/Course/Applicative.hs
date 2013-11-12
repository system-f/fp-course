module Course.Applicative(
 Applicative(..)
) where

import Course.Core
import Course.Apply
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class Apply f => Applicative f where
  pure ::
    a -> f a

-- todo fix Exercise
-- Exercise 6
-- Relative Difficulty: 3
-- (use bind and return)
--
-- | Witness that all things with bind and return also have fmap.
--
-- >>> fmap' (+1) (Id 2)
-- Id 3
--
-- >>> fmap' (+1) Nil
-- []
--
-- >>> fmap' (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$>) ::
  (a -> b)
  -> f a
  -> f b
(<$>) =
  error "todo"

instance Applicative Id where
  pure =
    error "todo"

instance Applicative List where
  pure =
    error "todo"

instance Applicative Optional where
  pure =
    error "todo"

instance Applicative ((->) t) where
  pure =
    error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return

instance Applicative [] where
  pure =
    P.return

instance Applicative P.Maybe where
  pure =
    P.return
