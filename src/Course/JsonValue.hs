{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.JsonValue where

import Course.Core
import Course.List

type Assoc = List (Chars, JsonValue)

data JsonValue where
   JsonString   :: Chars              -> JsonValue
   JsonRational :: Bool  -> !Rational -> JsonValue
   JsonObject   :: Assoc              -> JsonValue
   JsonArray    :: List JsonValue     -> JsonValue
   JsonTrue     :: JsonValue
   JsonFalse    :: JsonValue
   JsonNull     :: JsonValue
  deriving (Show, Eq)
