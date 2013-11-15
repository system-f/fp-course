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

instance Comonad Id where
  copure (Id a) =
    a

-- | Witness that all things with (<<=) and copure also have (<$>).
(<$>) ::
  Comonad f =>
  (a -> b)
  -> f a
  -> f b
(<$>) =
  undefined

