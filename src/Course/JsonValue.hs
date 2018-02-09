{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.JsonValue where

import Course.Core
import Course.List

type Assoc = List (Chars, JsonValue)

data JsonValue =
     JsonString Chars
   | JsonRational Rational
   | JsonObject Assoc
   | JsonArray  (List JsonValue)
   | JsonTrue
   | JsonFalse
   | JsonNull
  deriving (Show, Eq)
