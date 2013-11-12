module Course.Wrapped where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import qualified Prelude as P
import qualified Control.Applicative as A

newtype Wrapped f a =
  Wrapped {
    unwrap ::
      f a
  }

instance Functor f => P.Functor (Wrapped f) where
  f `fmap` Wrapped x =
    Wrapped (f <$> x)

instance Applicative f => A.Applicative (Wrapped f) where
  Wrapped f <*> Wrapped x =
    Wrapped (f <*> x)
  pure =
    Wrapped . pure

instance Monad f => P.Monad (Wrapped f) where
  Wrapped a >>= f =
    Wrapped (unwrap . f =<< a)
  return =
    Wrapped . pure
