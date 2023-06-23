{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor k where
  -- Pronounced, eff-map.
  (<$>) :: (a -> b) -> k a -> k b

effymap :: Functor k => (a -> b) -> k a -> k b
effymap = ((((((<$>))))))

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the ExactlyOne functor.
--
-- >>> (+1) <$> ExactlyOne 2
-- ExactlyOne 3
instance Functor ExactlyOne where
  (<$>) ::
    (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<$>) =
    error "todo: Course.Functor (<$>)#instance ExactlyOne"

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) :: (a -> b) -> List a -> List b
  (<$>) =
    map

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) :: (a -> b) -> Optional a -> Optional b
  (<$>) =
    mapOptional

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
-- covariant
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
  (<$>) =
    error "todo: Course.Functor (<$>)#((->) t)"

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
(<$) ::
  Functor k =>
  a -> k b -> k a
(<$) a kb =
  error "bug"

blah = ((+) 2) 2

data List2 a = List2

data Banana
data Orange

sortList :: Ord a => List a -> List a
sortList = error "all done, somewhere I call compare func"

sortListBanana ::
  List Banana -> List Banana
sortListBanana = error "all done"

sortListOrange ::
  List Orange -> List Orange
sortListOrange = error "all done"


-- | Apply a value to a functor-of-functions.
--
-- __NOTE__: The second argument is a bare @a@, not a @k a@. You need
-- a more powerful typeclass, 'Applicative', if you want both the
-- functions and the argmuents to be "inside" the Functor:
--
-- @
-- (<*>) :: Applicative k => k (a -> b) -> k a -> k b
-- @
--
-- We will talk about 'Applicative' soon.
--
-- >>> (*2) :. (+1) :. const 99 :. Nil ?? 8
-- [16,9,99]
--
-- >>> Empty ?? 2
-- Empty
(??) ::
  Functor k =>
  k (a -> b) -> a -> k b
(??) ff a =
  error "todo: Course.Functor#(??)"

infixl 1 ??

-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor k =>
  k a
  -> k ()
void ka =
  () <$ ka

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
