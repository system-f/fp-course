{-# LANGUAGE NoImplicitPrelude #-}

module Course.Bind(
  Bind(..)
, (>>=)
, join
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

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
(<*>) ::
  Bind f =>
  f (a -> b)
  -> f a
  -> f b
(<*>) =
  error "todo"

-- | Binds a function on the Id monad.
--
-- >>> bind (\x -> Id(x+1)) (Id 2)
-- Id 3
instance Bind Id where
  (=<<) =
    error "todo"

-- | Binds a function on a List.
--
-- >>> bind (\n -> n :. n :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Bind List where
  (=<<) =
    error "todo"

-- | Binds a function on an Optional.
--
-- >>> bind (\n -> Full (n + n)) (Full 7)
-- Full 14
instance Bind Optional where
  (=<<) =
    error "todo"

-- | Binds a function on the reader ((->) t).
--
-- >>> bind (*) (+10) 7
-- 119
instance Bind ((->) t) where
  (=<<) =
    error "todo"

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Bind f =>
  f (f a)
  -> f a
join =
  error "todo"

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
(>>=) ::
  Bind f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) =
  error "todo"

infixl 1 >>=

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
