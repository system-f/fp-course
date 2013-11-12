module Course.Apply where

import Course.Core
import Course.Functor
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class Functor f => Apply f where
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

instance Apply Id where
  (<*>) =
    error "todo"

instance Apply List where
  (<*>) =
    error "todo"

instance Apply Optional where
  (<*>) =
    error "todo"

instance Apply ((->) t) where
  (<*>) =
    error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Apply IO where
  f <*> a =
    f P.>>= \f' -> P.fmap (f' $) a

instance Apply [] where
  f <*> a =
    f P.>>= \f' -> P.fmap (f' $) a

instance Apply P.Maybe where
  f <*> a =
    f P.>>= \f' -> P.fmap (f' $) a
