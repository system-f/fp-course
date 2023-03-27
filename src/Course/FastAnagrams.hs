{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.FastAnagrams where

import Course.Core
import Course.Functor
import Course.List
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams :: Chars -> FilePath -> IO (List Chars)
fastAnagrams =
    error "todo: Course.FastAnagrams#fastAnagrams"

newtype NoCaseString
    = NoCaseString
        Chars

ncString :: NoCaseString -> Chars
ncString (NoCaseString s) =
    s

instance Eq NoCaseString where
    (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
    show = show . ncString
