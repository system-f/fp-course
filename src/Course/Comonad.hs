{-# LANGUAGE NoImplicitPrelude #-}

module Course.Comonad
(
  Comonad(..)
) where

import Course.Core
import Course.Extend
import Course.Id

class Extend f => Comonad f where
  copure ::
    f a
    -> a

-- | Implement the @Comonad@ instance for @Id@.
--
-- >>> copure (Id 7)
-- 7
instance Comonad Id where
  copure (Id a) =
    a

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- (+10) <$> Id 7
-- 17
(<$>) ::
  Comonad f =>
  (a -> b)
  -> f a
  -> f b
f <$> a =
  f . copure <<= a
