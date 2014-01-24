{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  copure =
    error "todo"

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
  error "todo"
