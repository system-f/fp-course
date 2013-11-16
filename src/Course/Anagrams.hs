{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Str
  -> Filename
  -> IO (List Str)
anagrams name =
  (<$>) (intersectBy equalIgnoringCase (permutations name) . lines) . readFile

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Str
  -> Str
  -> Bool
equalIgnoringCase =
  (==) `on` map toLower
