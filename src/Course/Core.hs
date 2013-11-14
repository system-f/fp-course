{-# LANGUAGE NoImplicitPrelude #-}

module Course.Core(
  Eq(..)
, Ord(..)
, Show(..)
, Enum(..)
, Integral(..)
, Bounded(..)
, RealFrac(..)
, Num(..)
, Bool(..)
, Either(..)
, Int
, Integer
, IO
, Rational
, seq
, error
, undefined
, const
, flip
, id
, otherwise
, (.)
, ($)
, (&&)
, (||)
, not
, even
, odd
, fst
, snd
, getChar
, on
, IsString(..)
, module Data.Char
) where


import Prelude(
    Eq(..)
  , Ord(..)
  , Show(..)
  , Enum(..)
  , Integral(..)
  , Bounded(..)
  , RealFrac(..)
  , Num(..)
  , Bool(..)
  , Either(..)
  , Char
  , Int
  , Integer
  , IO
  , Rational
  , seq
  , error
  , undefined
  , const
  , flip
  , id
  , otherwise
  , (.)
  , ($)
  , (&&)
  , (||)
  , not
  , even
  , odd
  , fst
  , snd
  )
import Data.String(
  IsString(..)
  )

import System.IO(
    getChar
  )
import Data.Function(
    on
  )
import Data.Char
