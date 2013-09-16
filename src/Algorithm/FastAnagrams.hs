module Algorithm.FastAnagrams where

import Data.Char(toLower)
import Data.Function(on)
import qualified Data.Set as S(member, fromList)
import Data.List(permutations)

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  String
  -> FilePath
  -> IO [String]
fastAnagrams name f =
  (flip (filter . flip S.member) (permutations name) . S.fromList . lines) `fmap` readFile f

newtype NoCaseString =
  NoCaseString {
    ncString :: String
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
