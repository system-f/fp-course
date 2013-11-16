{-# LANGUAGE NoImplicitPrelude #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Str
  -> Filename
  -> IO (List Str)
fastAnagrams name f =
  (flip (filter . flip S.member) (permutations name) . S.fromList . hlist . lines) <$> readFile f

newtype NoCaseString =
  NoCaseString {
    ncString :: Str
  }

instance Eq NoCaseString where
  (==) =
    (==) `on` (<$>) toLower . ncString

instance Show NoCaseString where
  show =
    show . ncString
