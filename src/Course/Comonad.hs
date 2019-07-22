{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Comonad where

import Course.Core
import Course.ExactlyOne
import Course.Extend

-- | All instances of the `Comonad` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. copure <<= x ≅ x`
--
-- * The law of right identity
--   `∀f. copure . (f <<=) == f
class Extend k => Comonad k where
  copure ::
    k a
    -> a

-- | Implement the @Comonad@ instance for @ExactlyOne@.
--
-- >>> copure (ExactlyOne 7)
-- 7
instance Comonad ExactlyOne where
  copure ::
    ExactlyOne a
    -> a
  copure =
    error "todo: Course.Comonad copure#instance ExactlyOne"

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- >>> (+10) <$$> ExactlyOne 7
-- ExactlyOne 17
(<$$>) ::
  Comonad k =>
  (a -> b)
  -> k a
  -> k b
(<$$>) =
  error "todo: Course.Comonad#(<$>)"
