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
class Functor f where
  -- Pronounced, eff-map.
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

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
  (<$>) :: (a -> b) -> ExactlyOne a -> ExactlyOne b
  (<$>) f (ExactlyOne a) = ExactlyOne (f a)

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) :: (a -> b) -> List a -> List b
  (<$>) _ Nil = Nil
  (<$>) f (h :. t) = (f h) :. ((<$>) f t)

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) :: (a -> b) -> Optional a -> Optional b
  (<$>) _ Empty = Empty
  (<$>) f (Full a) = Full (f a)

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) :: (a -> b) -> ((->) t a) -> ((->) t b)
  (<$>) f g = \x -> f (g x)

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
(<$) :: Functor f => a -> f b -> f a
(<$) a = (<$>) (const a)

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
void :: Functor f => f a -> f ()
void = (<$) ()

-- Questions
--   `∀f g x. (f . g <$> x) ≅ (f <$> (g <$> x))`
--
-- How do I read this?
-- "Applying first g and then f, I get the same result as applying f . g" but
-- this course hasn't explained what . is yet so am I meant to already know or
-- not?
--
-- Why is it <$> why not <$#$#%%> - are they arrows pointing at either side? Why
-- dollar? <$> It says it's pronounced eff-map so could I define (effmap)
-- so I could write (g effmap x)?

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
