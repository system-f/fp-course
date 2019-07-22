{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Contravariant where

import Course.Core

-- | A 'Predicate' is usually some kind of test about a
-- thing. Example: a 'Predicate Integer' says "give me an 'Integer'"
-- and I'll answer 'True' or 'False'.
data Predicate a = Predicate (a -> Bool)

runPredicate ::
  Predicate a
  -> a
  -> Bool
runPredicate (Predicate f) =
  f

-- | A 'Comparison' looks at two things and says whether the first is
-- smaller, equal to, or larger than the second. 'Ordering' is a
-- three-valued type used as the result of a comparison, with
-- constructors 'LT', 'EQ', and 'GT'.
data Comparison a = Comparison (a -> a -> Ordering)

runComparison ::
  Comparison a
  -> a
  -> a
  -> Ordering
runComparison (Comparison f) =
  f

-- | All this type does is swap the arguments around. We'll see why we
-- want it when we look at its 'Contravariant' instance.
data SwappedArrow a b = SwappedArrow (b -> a)

runSwappedArrow ::
  SwappedArrow a b
  -> b
  -> a
runSwappedArrow (SwappedArrow f) = f

-- | All instances of the `Contravariant` type-class must satisfy two
-- laws. These laws are not checked by the compiler. These laws are
-- given as:
--
-- * The law of identity
--   `∀x. (id >$< x) ≅ x`
--
-- * The law of composition
--   `∀f g x. (g . f >$< x) ≅ (f >$< (g >$< x))`
--
-- If you think of a 'Functor' as "having" an @a@ that you map over,
-- you can think of a 'Contravariant' as "accepting" an @a@. So if you
-- can turn @b@ into @a@ (i.e., with the first argument to (>$<)')
-- then you can make your 'Contravariant' accept @b@ instead.
class Contravariant k where
  -- Pronounced, contramap.
  (>$<) ::
    (b -> a)
    -> k a
    -> k b

infixl 4 >$<

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import Prelude (length)

-- | Maps a function before a Predicate.
--
-- >>> runPredicate ((+1) >$< Predicate even) 2
-- False
instance Contravariant Predicate where
  (>$<) ::
    (b -> a)
    -> Predicate a
    -> Predicate b
  (>$<) =
    error "todo: Course.Contravariant (>$<)#instance Predicate"

-- | Use the function before comparing.
--
-- >>> runComparison (show >$< Comparison compare) 2 12
-- GT
instance Contravariant Comparison where
  (>$<) ::
    (b -> a)
    -> Comparison a
    -> Comparison b
  (>$<) =
    error "todo: Course.Contravariant (>$<)#instance Comparison"

-- | The kind of the argument to 'Contravariant' is @Type -> Type@, so
-- our '(>$<)' only works on the final type argument. The
-- 'SwappedArrow' type reverses the arguments, which gives us the
-- right shape.
--
-- >>> runSwappedArrow (length >$< SwappedArrow (+10)) "hello"
-- 15
instance Contravariant (SwappedArrow t) where
  (>$<) ::
    (b -> a)
    -> SwappedArrow x a
    -> SwappedArrow x b
  (>$<) =
    error "todo: Course.Contravariant (>$<)#instance SwappedArrow"


-- | If we give our 'Contravariant' an @a@, then we can "accept" any
-- @b@ by ignoring it.
--
-- prop> \x -> runPredicate (3 >$ Predicate odd) x == True
(>$) ::
  Contravariant k =>
  a
  -> k a
  -> k b
(>$) =
  error "todo: Course.Contravariant#(>$)"