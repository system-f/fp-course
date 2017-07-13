{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Monad where

import Course.Applicative
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional
import qualified Prelude as P((=<<))

-- | All instances of the `Monad` type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   `∀f g x. g =<< (f =<< x) ≅ ((g =<<) . f) =<< x`
class Applicative f => Monad f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> ExactlyOne (+10) <**> ExactlyOne 8
-- ExactlyOne 18
--
-- >>> (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <**> Full 7
-- Full 15
--
-- >>> Empty <**> Full 7
-- Empty
--
-- >>> Full (+8) <**> Empty
-- Empty
--
-- >>> ((+) <**> (+10)) 3
-- 16
--
-- >>> ((+) <**> (+5)) 3
-- 11
--
-- >>> ((+) <**> (+5)) 1
-- 7
--
-- >>> ((*) <**> (+10)) 3
-- 39
--
-- >>> ((*) <**> (+2)) 3
-- 15
(<**>) ::
  Monad f =>
  f (a -> b)
  -> f a
  -> f b
(<**>) k a =
  -- (=<<)      :: (x -> f y) -> f x -> f y
  -- flip (=<<) :: f x -> (x -> f y) -> f y
  -- pure :: r -> f r

  -- k :: f (a -> b)
  -- a :: f a
  -- l :: a -> b
  -- x :: a
  -- l x :: b
  -- ? :: f b
  flip (=<<) k (\l ->
  flip (=<<) a (\x ->
  pure (l x)))

infixl 4 <**>

-- | Binds a function on the ExactlyOne monad.
--
-- >>> (\x -> ExactlyOne(x+1)) =<< ExactlyOne 2
-- ExactlyOne 3
instance Monad ExactlyOne where
  (=<<) ::
    (a -> ExactlyOne b)
    -> ExactlyOne a
    -> ExactlyOne b
  (=<<) =
    \f a -> case a of
              ExactlyOne x -> f x 

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Monad List where
  (=<<) ::
    (a -> List b)
    -> List a
    -> List b
  (=<<) f list =
  --foldRight (\a b -> f a ++ b) Nil list
  --foldRight ((++) . f) Nil list
    flatMap f list

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Monad Optional where
  (=<<) ::
    (a -> Optional b)
    -> Optional a
    -> Optional b
  (=<<) _ Empty =
    Empty
  (=<<) f (Full a) =
    f a

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Monad ((->) t) where
  (=<<) ::
    (a -> t -> b) -> (t -> a) -> t -> b
  (=<<) jason jamie pizza =
    jason (jamie pizza) pizza

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
  Monad f =>
  f (f a)
  -> f a
join =
-- GIVE ME THE ONLY PROGRAM THAT TYPE CHECKS
  (=<<) id

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Monad f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) =
  -- f <$> x :: f (f b)
-- \x f -> join ((<$>) f x)
  \x f -> (join . (<$>) f) x

data Hi f a =
  Hi (f (Optional a))

data Hihi t f a =
  Hihi (t -> f a)

instance (Functor f) => Functor (Hi f) where
  -- (a -> b) -> Hi f g a -> Hi f g b
  f <$> Hi x =
    Hi ((<$>) ((<$>) f) x)
-- \a -> f (g a)
-- f . g

-- Monads do not compose in the general case
instance (Applicative f) => Applicative (Hi f) where

instance (Monad f) => Monad (Hi f) where

infixl 1 >>=

-- | Implement composition within the @Monad@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Monad f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c
(<=<) wossitupthere can'tthinkofanything um =
  wossitupthere =<< can'tthinkofanything um

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where
  (=<<) =
    (P.=<<)
