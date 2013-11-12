module Course.Bind(
  Bind(..)
, (>>=)
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

-- todo exercise
(<*>) ::
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

instance Bind Id where
  (=<<) =
    error "todo"

instance Bind List where
  (=<<) =
    error "todo"

instance Bind Optional where
  (=<<) =
    error "todo"

instance Bind ((->) t) where
  (=<<) =
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
