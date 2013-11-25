{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Monad where

import Course.Applicative
import Course.Bind
import Course.Core
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class (Applicative f, Bind f) => Monad f where

instance Monad Id where

instance Monad List where

instance Monad Optional where

instance Monad ((->) t) where

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Monad IO where

instance Monad [] where

instance Monad P.Maybe where
