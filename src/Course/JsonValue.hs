{-# LANGUAGE NoImplicitPrelude #-}

module Course.JsonValue where

import Course.Core
import Course.List

type Assoc = [(Str, JsonValue)]

data JsonValue =
     JsonString Str
   | JsonRational  Bool !Rational
   | JsonObject Assoc
   | JsonArray  [JsonValue]
   | JsonTrue
   | JsonFalse
   | JsonNull
  deriving (Show, Eq)
