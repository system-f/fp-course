{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Applicative(
  Applicative(..)
, lift2
, lift3
, lift4
, (*>)
, (<*)
, sequence
, replicateA
, filtering
, return
, fail
, (>>)
) where

import Course.Core
import Course.Functor hiding ((<$>))
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P(fmap, return, (>>=))

-- | All instances of the `Applicative` type-class must satisfy three laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of associative composition
--   `∀a b c. ((.) <$> a <*> b <*> c) ≅ (a <*> (b <*> c))`
--
-- * The law of left identity
--   `∀x. pure id <*> x ≅ x`
--
-- * The law of right identity
--   `∀x. x <*> pure id ≅ x`
class Functor f => Applicative f where
  pure ::
    a -> f a
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

infixl 4 <*>

-- | Witness that all things with (<*>) and pure also have (<$>).
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$>) ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
(<$>) =
  error "todo: Course.Applicative#(<$>)"

-- | Insert into Id.
--
-- prop> pure x == Id x
--
-- >>> Id (+10) <*> Id 8
-- Id 18
instance Applicative Id where
  pure ::
    a
    -> Id a
  pure =
    error "todo: Course.Applicative pure#instance Id"
  (<*>) :: 
    Id (a -> b)
    -> Id a
    -> Id b
  (<*>) =
    error "todo: Course.Applicative (<*>)#instance Id"

-- | Insert into a List.
--
-- prop> pure x == x :. Nil
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Applicative List where
  pure ::
    a
    -> List a
  pure =
    error "todo: Course.Applicative pure#instance List"
  (<*>) ::
    List (a -> b)
    -> List a
    -> List b
  (<*>) =
    error "todo: Course.Apply (<*>)#instance List"

-- | Insert into an Optional.
--
-- prop> pure x == Full x
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Applicative Optional where
  pure ::
    a
    -> Optional a
  pure =
    error "todo: Course.Applicative pure#instance Optional"
  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b
  (<*>) =
    error "todo: Course.Apply (<*>)#instance Optional"

-- | Insert into a constant function.
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
--
-- prop> pure x y == x
instance Applicative ((->) t) where
  pure ::
    a
    -> ((->) t a)
  pure =
    error "todo: Course.Applicative pure#((->) t)"
  (<*>) ::
    ((->) t (a -> b))
    -> ((->) t a)
    -> ((->) t b)
  (<*>) =
    error "todo: Course.Apply (<*>)#instance ((->) t)"


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
  Applicative f =>
  (a -> b -> c)
  -> f a
  -> f b
  -> f c
lift2 =
  error "todo: Course.Applicative#lift2"

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
  Applicative f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
lift3 =
  error "todo: Course.Applicative#lift3"

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
  Applicative f =>
  (a -> b -> c -> d -> e)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
lift4 =
  error "todo: Course.Applicative#lift4"

-- | Apply, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. Nil) *> (4 :. 5 :. 6 :. Nil)
-- [4,5,6,4,5,6]
--
-- >>> (1 :. 2 :. 3 :. Nil) *> (4 :. 5 :. Nil)
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> (a :. b :. c :. Nil) *> (x :. y :. z :. Nil) == (x :. y :. z :. x :. y :. z :. x :. y :. z :. Nil)
--
-- prop> Full x *> Full y == Full y
(*>) ::
  Applicative f =>
  f a
  -> f b
  -> f b
(*>) =
  error "todo: Course.Applicative#(*>)"

-- | Apply, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> (1 :. 2 :. Nil) <* (4 :. 5 :. 6 :. Nil)
-- [1,1,1,2,2,2]
--
-- >>> (1 :. 2 :. 3 :. Nil) <* (4 :. 5 :. Nil)
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> (x :. y :. z :. Nil) <* (a :. b :. c :. Nil) == (x :. x :. x :. y :. y :. y :. z :. z :. z :. Nil)
--
-- prop> Full x <* Full y == Full x
(<*) ::
  Applicative f =>
  f b
  -> f a
  -> f b
(<*) =
  error "todo: Course.Applicative#(<*)"

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (Id 7 :. Id 8 :. Id 9 :. Nil)
-- Id [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)
sequence =
  error "todo: Course.Applicative#sequence"

-- | Replicate an effect a given number of times.
--
-- >>> replicateA 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ('a' :. 'b' :. 'c' :. Nil)
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative f =>
  Int
  -> f a
  -> f (List a)
replicateA =
  error "todo: Course.Applicative#replicateA"

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (Id . even) (4 :. 5 :. 6 :. Nil)
-- Id [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering ::
  Applicative f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filtering =
  error "todo: Course.Applicative#filtering"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

return ::
  Applicative f =>
  a
  -> f a
return =
  pure

fail ::
  Applicative f =>
  Chars
  -> f a
fail =
  error . hlist

(>>) ::
  Applicative f =>
  f a
  -> f b
  -> f b
(>>) =
  (*>)
