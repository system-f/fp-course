module Network.Server.Common.Line where

import Data.Char(isSpace, toLower)
import Data.Function(on)
import Control.Monad.Trans(MonadIO(..))
import Control.Exception(IOException)

-- |
--
-- >>> trimPrefixThen "ABC" "AB"
-- Nothing
--
-- >>> trimPrefixThen "ABC" "ABC"
-- Just ""
--
-- >>> trimPrefixThen "ABC" "ABCDEF"
-- Just "DEF"
--
-- >>> trimPrefixThen "ABC" "Ab"
-- Nothing
--
-- >>> trimPrefixThen "ABC" "Abc"
-- Just ""
--
-- >>> trimPrefixThen "ABC" "Abcdef"
-- Just "def"
--
-- >>> trimPrefixThen "ABC" "Abcdef   ghi  "
-- Just "def   ghi"
trimPrefixThen ::
  String
  -> String
  -> Maybe String
trimPrefixThen l z =
  fmap (reverse . dropWhile isSpace . reverse . dropWhile isSpace) (prefixThen ((==) `on` toLower) l z)

-- |
--
-- >>> prefixThen (==) "ABC" "AB"
-- Nothing
--
-- >>> prefixThen (==) "ABC" "ABC"
-- Just ""
--
-- >>> prefixThen (==) "ABC" "ABCDEF"
-- Just "DEF"
prefixThen ::
  (a -> a -> Bool)
  -> [a]
  -> [a]
  -> Maybe [a]
prefixThen _ [] r =
  Just r
prefixThen _ _ [] =
  Nothing
prefixThen e (a:b) (c:d) =
  if e a c
    then
      prefixThen e b d
    else
      Nothing

xprint ::
  MonadIO m =>
  IOException
  -> m ()
xprint =
  liftIO . print
