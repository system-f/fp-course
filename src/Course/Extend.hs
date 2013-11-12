module Course.Extend where

import Course.Core
import Course.Id
import Course.List
import Course.Optional
import Course.Functor

class Functor f => Extend f where
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b

infixr 1 <<=

instance Extend Id where
  f <<= i =
    Id (f i)

instance Extend List where
  _ <<= Nil =
    Nil
  f <<= x@(_ :. t) =
    f x :. (f <<= t)

instance Extend Optional where
  f <<= o =
    f . Full <$> o
