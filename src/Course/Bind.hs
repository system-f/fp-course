module Course.Bind(
  Bind(..)
, (>>=)
, flatten'
) where

import Course.Core
import Course.Apply
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class Apply f => Bind f where
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- todo exercise
(<*>) ::
  Bind f =>
  f (a -> b)
  -> f a
  -> f b
(<*>) =
  error "todo"

(>>=) ::
  Bind f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) =
  flip (=<<)

infixr 1 >>=

-- Exercise 7
--
-- | Binds a function on the Id monad.
--
-- >>> bind (\x -> Id(x+1)) (Id 2)
-- Id 3
instance Bind Id where
  (=<<) =
    error "todo"

-- Exercise 8
--
-- | Binds a function on a List.
--
-- >>> bind (\n -> n :. n :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Bind List where
  (=<<) =
    error "todo"

-- Exercise 9
--
-- | Binds a function on an Optional.
--
-- >>> bind (\n -> Full (n + n)) (Full 7)
-- Full 14
instance Bind Optional where
  (=<<) =
    error "todo"

-- Exercise 10
--
-- | Binds a function on the reader ((->) t).
--
-- >>> bind (*) (+10) 7
-- 119
instance Bind ((->) t) where
  (=<<) =
    error "todo"

-- Exercise 11
--
-- | Flattens a combined structure to a single structure.
--
-- >>> flatten' ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> flatten' (Full Empty)
-- Empty
--
-- >>> flatten' (Full (Full 7))
-- Full 7
--
-- >>> flatten' (+) 7
-- 14
flatten' ::
  Bind f =>
  f (f a)
  -> f a
flatten' =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Bind IO where
  (=<<) =
    (P.=<<)

instance Bind [] where
  (=<<) =
    (P.=<<)

instance Bind P.Maybe where
  (=<<) =
    (P.=<<)
