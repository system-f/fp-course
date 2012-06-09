module L09.BKTree
(
  BKTree
, empty
, bktree
, isEmpty
, null
, size
, (.:.)
, member
, withinDistance
, fromWords
, fromDictionaryFile
) where

import L09.MetricSpace
import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (any, foldr)
import Data.Foldable
import Data.Monoid

data BKTree a =
  Node a !Int (BMap a)
  | Leaf
  deriving (Eq, Show)

empty ::
  BKTree a
empty =
  error "todo"

instance MetricSpace a => Monoid (BKTree a) where
  mempty =
    error "todo"
  mappend =
    error "todo"
  mconcat =
    error "todo"

instance Foldable BKTree where
  foldl =
    error "todo"
  foldr =
    error "todo"

bktree ::
  (MetricSpace a, Foldable f) =>
  f a
  -> BKTree a
bktree =
  error "todo"

isEmpty ::
  BKTree a
  -> Bool
isEmpty =
  error "todo"

size ::
  BKTree a
  -> Int
size =
  error "todo"

(.:.) ::
  MetricSpace a =>
  a
  -> BKTree a
  -> BKTree a
(.:.) =
  error "todo"

infixr 5 .:.

member ::
  MetricSpace a =>
  a
  -> BKTree a
  -> Bool
member =
  error "todo"

withinDistance ::
  MetricSpace a =>
  Int
  -> a
  -> BKTree a
  -> [(Int, a)]
withinDistance =
  error "todo"

fromWords ::
  String
  -> BKTree String
fromWords =
  error "todo"

fromDictionaryFile ::
  FilePath
  -> IO (BKTree String)
fromDictionaryFile =
  error "todo"

-- not exported

type BMap a =
  Map Int (BKTree a)

asList ::
  BKTree a
  -> [a]
asList Leaf =
  []
asList (Node a _ m) =
  a : (fmap snd (M.toList m) >>= asList)

usingMap ::
  (BMap a -> x)
  -> x
  -> BKTree a
  -> x
usingMap _ l Leaf =
  l
usingMap f _ (Node _ _ m) =
  f m

breakMap ::
  Int
  -> Int
  -> BMap a
  -> BMap a
breakMap d n m =
  fst $ M.split (d + n + 1) (snd $ M.split (d - n - 1) m)
