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

{-

The only exercise here is a thinking one. The understanding that the Monad
type-class is the coming together of its sub type-classes
(`Applicative` and `Bind`). There are no coding exercises here. The purpose of 
this module is simply to provide a definition for the word "monad" and that
definition is built on previous exercises.

The monad type-class provides no additional methods to `Applicative` and `Bind`.

-}

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
