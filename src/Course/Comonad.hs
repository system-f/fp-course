{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Comonad
(
  Comonad(..)
) where

import Course.Core
import Course.Extend
import Course.Id

-- | All instances of the `Comonad` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. copure <<= x ≅ x`
--
-- * The law of right identity
--   `∀f. copure . (f <<=) == f
class Extend f => Comonad f where
  copure ::
    f a
    -> a

-- | Implement the @Comonad@ instance for @Id@.
--
-- >>> copure (Id 7)
-- 7
instance Comonad Id where
  copure ::
    Id a
    -> a
  copure =
    error "todo: Course.Comonad copure#instance Id"

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- >>> (+10) <$> Id 7
-- Id 17
(<$>) ::
  Comonad f =>
  (a -> b)
  -> f a
  -> f b
(<$>) =
  error "todo: Course.Comonad#(<$>)"
