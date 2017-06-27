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
 -- (a -> b) -> Int a -> Int b -- WTF, kind error

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
  (<$>) k (ExactlyOne x) =
    ExactlyOne (k x)

-- k :: a -> b
-- x :: a
-- k x :: b
-- ExactlyOne (k x) :: ExactlyOne b
-- ? :: ExactlyOne b

-- data ExactlyOne a = ExactlyOne a

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
  (<$>) f =
    foldRight ((:.) . f) Nil

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
  (<$>) f =
    bindOptional (Full . f)

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)-> (t -> a) -> (t -> b)
  (<$>)  a b c = a (b c) -- \joushau -> \shujoa -> \hai -> joushau (shujoa hai)
    -- joushau :: a -> b
    -- shujoa :: t -> a
    -- hai :: t
    -- ? :: b

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> x <$ Full q == Full x
(<$) ::
  Functor f => a-> f b -> f a
(<$) =
  -- \b -> (<$>) (const b)
  (<$>) . const

anonymousmapOptional :: a-> Optional b -> Optional a
anonymousmapOptional =
  -- \b -> (<$>) (const b)
  mapOptional . const

anonymousmapList :: a-> List b -> List a
anonymousmapList =
  -- \b -> (<$>) (const b)
  map . const

-- (<$) b c = (<$>) (\ _ -> b) c

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
  Functor f =>
  f a
  -> f ()
void =
  (() <$)

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
