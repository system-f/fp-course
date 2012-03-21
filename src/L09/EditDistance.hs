module L09.EditDistance
(
  editDistance
, Edit(..)
, Edits
, diff
, applyDiff
) where

import Data.Array
import Data.Foldable hiding (minimum)

editDistance ::
  Eq a =>
  [a]
  -> [a]
  -> Int
editDistance =
  error "todo"

data Edit a =
  Delete
  | Insert a
  | Subst a
  | Copy
  deriving (Eq, Show)

type Edits a =
  [Edit a]

diff ::
  Eq a =>
  [a]
  -> [a]
  -> Edits a
diff =
  error "todo"

applyDiff ::
  [a]
  -> Edits a
  -> [a]
applyDiff =
  error "todo"

-- do not export

table ::
  Eq a =>
  [a]
  -> [a]
  -> Array (Int, Int) Int
table xs ys  =
  let m      = length xs
      n      = length ys
      k i s  = (1,i) `array` zip [1..] s
      x      = k m xs
      y      = k n ys

      t      = b `array` [(z, distance z) | z <- range b]
      b      = ((0,0),(m,n))

      distance (0,j) = j
      distance (i,0) = i
      distance (i,j) =
        let track = [(1,0,1),(0,1,1),(1,1, if x ! i == y ! j then 0 else 1)]
        in minimum . fmap (\(p, q, o) -> t ! (i-p,j-q) + o) $ track
  in t
