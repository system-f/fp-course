module Course.Monad where

import Course.Applicative
import Course.Bind

class (Applicative f, Bind f) => Monad f where
