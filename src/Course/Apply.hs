{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Apply where

import Course.Core
import Course.Functor
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P


-- | All instances of the `Apply` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associative composition
--   `∀a b c. ((.) <$> a <*> b <*> c) ≅ (a <*> (b <*> c))`
class Functor f => Apply f where
  -- Pronounced apply.
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

infixl 4 <*>

-- | Implement @Apply@ instance for @Id@.
--
-- >>> Id (+10) <*> Id 8
-- Id 18
instance Apply Id where
  (<*>) ::
    Id (a -> b)
    -> Id a
    -> Id b
  Id f <*> Id a =
    Id (f a)

-- | Implement @Apply@ instance for @List@.
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Apply List where
  (<*>) ::
    List (a -> b)
    -> List a
    -> List b
  -- flatMap :: (a -> List b) -> List a -> List b
  listf <*> lista =
    flMap listf (\f ->
    flMap lista (\a -> 
    f a :. Nil))
    
    {-
    f <- listf
    a <- lista
    pure (f a)

for {
  f <- listf
  a <- list a
} yield f a

from f in listf
from a in lista
select f(a)

    -}
flMap :: List x -> (x -> List y) -> List y
flMap = flip flatMap

    {-
  _ <*> Nil =
    Nil
  Nil <*> _ =
    Nil
  (f:.fs) <*> q@(_:._) =
    map (\a -> f a) q ++
    (fs <*> q)
-}

-- | Implement @Apply@ instance for @Optional@.
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Apply Optional where
  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b
  optionalf <*> optionala =
    bindOpt2 optionalf (\f ->
    bindOpt2 optionala (\a -> 
    Full (f a)))

  {-

bindOptional ::
  (a -> Optional b) -> Optional a -> Optional b
-}

bindOpt2 :: 
  Optional a -> (a -> Optional b) -> Optional b
bindOpt2 =
  flip bindOptional

-- | Implement @Apply@ instance for reader.
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
instance Apply ((->) t) where
  {-
  (<*>) ::
    ((->) t (a -> b))
    -> ((->) t a)
    -> ((->) t b)
  -}
  (<*>) ::
    (t -> a -> b) -- f
    -> (t -> a)   -- g
    -> t          -- t
    -> b
  (<*>) x y z =
    x   z ( y   z)
    
-- Sxyz = xz(yz)

flbindReader ::
  (t -> a)
  -> (a -> t -> b)
  -> t
  -> b
flbindReader f g t =
  g (f t) t

pureReader ::
  a
  -> t
  -> a
pureReader = const

-- | Apply a binary function in the environment.
--
-- >>> lift2 (+) (Id 7) (Id 8)
-- Id 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18
lift2 ::
  Apply f => -- has (<$>) and (<*>)
  (a -> b -> c) -- f
  -> f a        -- a
  -> f b        -- b
  -> f c
lift2 f a b =
  f <$> a <*> b

-- f <$> a :: ?
-- a) type error
-- b) something else :: f (b -> c)

-- | Apply a ternary function in the environment.
--
-- >>> lift3 (\a b c -> a + b + c) (Id 7) (Id 8) (Id 9)
-- Id 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  Apply f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
lift3 f a b c =
  f <$> a <*> b <*> c

-- | Apply a quaternary function in the environment.
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Id 7) (Id 8) (Id 9) (Id 10)
-- Id 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Apply f =>
  (a -> b -> c -> d -> e)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
lift4 f a b c d =
  f <$> a <*> b <*> c <*> d

-- | Sequence, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> [1,2,3] *> [4,5,6]
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> [1,2] *> [4,5,6]
-- [4,5,6,4,5,6]
--
-- >>> [1,2,3] *> [4,5]
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> [a,b,c] *> [x,y,z] == [x,y,z,x,y,z,x,y,z]
--
-- prop> Full x *> Full y == Full y
(*>) ::
  Apply f =>
  f a
  -> f b
  -> f b
(*>) =
  lift2 (const id)


-- | Sequence, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> [1,2,3] <* [4,5,6]
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> [1,2] <* [4,5,6]
-- [1,1,1,2,2,2]
--
-- >>> [1,2,3] <* [4,5]
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> [x,y,z] <* [a,b,c] == [x,x,x,y,y,y,z,z,z]
--
-- prop> Full x <* Full y == Full x
(<*) ::
  Apply f =>
  f b
  -> f a
  -> f b
(<*) =
  lift2 const

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Apply IO where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

instance Apply [] where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

instance Apply P.Maybe where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

(>>) ::
  Apply f =>
  f a
  -> f b
  -> f b
(>>) =
  (*>)
