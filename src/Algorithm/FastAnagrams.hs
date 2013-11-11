{-# LANGUAGE NoImplicitPrelude #-}

module Algorithm.FastAnagrams where

import Core
import Data.Char
import Data.Function
import qualified Data.Set as S
import Data.List

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  String
  -> FilePath
  -> IO [String]
fastAnagrams =
  error "todo"

newtype NoCaseString =
  NoCaseString {
    ncString :: String
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
