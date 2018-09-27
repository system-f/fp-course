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
  (<$>) ::
    (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  (<$>) f (ExactlyOne x) = ExactlyOne (f x)


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
  (<$>) _ Nil = Nil
  (<$>) f (x :. xs) = f x :. (f <$> xs)

--   JDL :
-- (<$>) f (headItem :. tailItems) =
--      let headMappedItem = f x
--          tailMappedItems = f <$> xs
--      in headMappedItem :. tailMappedItems

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
  (<$>) _ Empty = Empty
  (<$>) f (Full x) = Full (f x)

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
  (<$>) f g = f . g


-- JDL : alternate version: (<$>) f g x = f (g x)
-- or `(<$>) f g x = f $ g x`
-- or `(<$>) = (.)`
-- where does the `x` come from, you might wonder? If we look at the type signature:
-- (a -> b) -> ((->) t a) -> ((->) t b)
-- we can rewrite it as (a -> b) -> (t -> a) -> (t -> b)
-- and as `(->)` is right-associative, we can rewrite it like this:
-- (a -> b) -> (t -> a) -> t -> b
-- so, it could be viewed as a function of two arguments, both of whom are functions, that returns
-- a function (of type (t -> b)), but..., given functions are curried, that could also be
-- viewed as a function of three arguments that returns a **value** (of type b):
-- the first of which is a function, the second of which is a function, and the third of which is a value
-- of type `a`.
-- So, that `x` above is actually the first argument to the "return function". These are the two ways of
-- looking at curried functions. That is, `f :: a -> b` can be viewed as both a function value whose type is
-- `a -> b`, and also as a function that produces a `b` from an `a`, because functions are "built in" to Haskell,
-- this can be tricky to think about and understand.

-- JDL : alternate version for greater understandability:
-- note: probably need to motivate what reader is... in that it's an environment, etc. a function is a mapping of a value out of another value.
-- note: you have to understand what composition is to understand this one (that is, composition **is** function application)
-- might be worth explaining what point-free means, too
-- (<$>) functionFromValuesOfTypeAToValuesOfTypeB functionFromValuesOfTypeTToValuesOfTypeA =
--    \valueOfTypeT -> let valueOfTypeA = functionFromValuesOfTypeTToValuesOfTypeA valueOfTypeT in functionFromValuesOfTypeAToValuesOfTypeB valueOfTypeA

-- This is particularly fascinating because it "LIFTs" the `a -> b` function "across the context" of a function (t -> a)
-- that is, it takes an `a -> b` and returns an `f a -> f b` where `f` is a function that can do something.
-- the lifting is applying a function to the return value of the function... (ie inside the `f` context) (is another way to see it).

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
(<$) x fb =
  const x <$> fb

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
void = (<$) ()

-- JDL... alternates:
-- void fa = const () <$> fa
--
-- void = (<$>) (const ())
--
-- void fa = (\_ -> ()) <$> fa
--
-- void = (() <$)
--
-- void fb = () <$ fb

-- JDL alternate:
-- void f _ = ()
-- void = const ()

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
