module L09.EditDistance
(
  editDistance
, Edit(..)
, Edits
, diff
, applyDiff
) where

import Data.Array
import Prelude hiding (any, minimum)
import Data.Foldable

editDistance ::
  Eq a =>
  [a]
  -> [a]
  -> Int
editDistance x y =
  let t                    = table x y
      ((x0, y0), (x1, y1)) = bounds t
  in t ! (x1 - x0, y1 - y0)

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
diff a b =
  let di _ _ 0 0 = []
      di _ d 0 _ = map Insert d
      di _ _ n 0 = replicate n Delete
      di c d p q = let n = t ! (p, q)
                       n' = n - 1
                       o = t ! (p - 1, q - 1)
                       (g, h, i, j, k) = if n' == o
                                           then
                                             (Subst (head d), drop 1, drop 1, 1, 1)
                                           else
                                             if n' == t ! (p - 1, q)
                                               then
                                                 (Delete, drop 1, id, 1, 0)
                                               else
                                                 if n' == t ! (p, q - 1)
                                                   then
                                                     (Insert (head d), id, drop 1, 0, 1)
                                                   else
                                                     (Copy, drop 1, drop 1, 1, 1)
                   in g : di (h c) (i d) (p - j) (q - k)
      t = table a b
      a' = reverse a
      b' = reverse b
      ((x0, y0), (x1, y1)) = bounds t
  in reverse $ di a' b' (x1 - x0) (y1 - y0)

applyDiff ::
  [a]
  -> Edits a
  -> [a]
applyDiff _ [] =
  []
applyDiff [] _ =
  []
applyDiff (x:xs) (Copy:es) =
  x:applyDiff xs es
applyDiff (_:xs) (Subst c:es) =
  c:applyDiff xs es
applyDiff (_:xs) (Delete:es) =
  applyDiff xs es
applyDiff xs     (Insert c:es) =
  c:applyDiff xs es



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
