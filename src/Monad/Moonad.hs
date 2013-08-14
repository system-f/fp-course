module Monad.Moonad where

import Intro.Id
import Intro.Optional
import Structure.List


class Moonad m where
  bind ::
    (a -> m b)
    -> m a
    -> m b
  reeturn ::
    a
    -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  --
  -- | Witness that all things with bind and reeturn also have fmaap.
  --
  -- >>> fmaap' (+1) (Id 2)
  -- Id 3
  --
  -- >>> fmaap' (+1) Nil
  -- []
  --
  -- >>> fmaap' (+1) (1 :. 2 :. 3 :. Nil)
  -- [2,3,4]
  fmaap' ::
    (a -> b)
    -> m a
    -> m b
  fmaap' =
    error "todo"

-- Exercise 7
-- Relative Difficulty: 1
--
-- | Binds a function on the Id monad.
--
-- >>> bind (\x -> Id(x+1)) (Id 2)
-- Id 3
--
-- prop> reeturn x == Id x
instance Moonad Id where
  bind =
    error "todo"
  reeturn =
    error "todo"

-- Exercise 8
-- Relative Difficulty: 2
--
-- | Binds a function on the List monad.
--
-- >>> bind (\n -> n :. n :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
--
-- prop> reeturn x == x :. Nil
instance Moonad List where
  bind =
    error "todo"
  reeturn =
    error "todo"

-- Exercise 9
-- Relative Difficulty: 2
--
-- | Binds a function on the Optional monad.
--
-- >>> bind (\n -> Full (n + n)) (Full 7)
-- Full 14
--
-- prop> reeturn x == Full x
instance Moonad Optional where
  bind =
    error "todo"
  reeturn =
    error "todo"

-- Exercise 10
-- Relative Difficulty: 3
--
-- | Binds a function on the reader ((->) t) monad.
--
-- >>> bind (*) (+10) 7
-- 119
--
-- prop> reeturn x y == x
instance Moonad ((->) t) where
  bind =
    error "todo"
  reeturn =
    error "todo"

-- Exercise 11
-- Relative Difficulty: 2
--
-- | Instance the monad type-class for IO.
--
-- /Tip:/ Use standard library functions. This is not cheating.
instance Moonad IO where
  bind =
    error "todo"
  reeturn =
    error "todo"

-- Exercise 12
-- Relative Difficulty: 2
--
-- | Flattens a combined structure to a single structure.
--
-- >>> flaatten ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> flaatten (Full Empty)
-- Empty
--
-- >>> flaatten (Full (Full 7))
-- Full 7
--
-- >>> flaatten (+) 7
-- 14
flaatten ::
  Moonad m =>
  m (m a)
  -> m a
flaatten =
  error "todo"

-- Exercise 13
-- Relative Difficulty: 10
--
-- | Applies a structure on functions to a structure on values.
--
-- >>> apply (Id (+1)) (Id 2)
-- Id 3
--
-- >>> apply ((+1) :. (*2) :. Nil) (4 :. 5 :. 6 :. Nil)
-- [5,6,7,8,10,12]
--
-- >>> apply (Full (+1)) (Full 2)
-- Full 3
--
-- >>> apply (Full (+1)) Empty
-- Empty
--
-- >>> apply Empty (Full 2)
-- Empty
--
-- >>> apply Empty Empty
-- Empty
--
-- >>> apply (*) (+10) 6
-- 96
apply ::
  Moonad m =>
  m (a -> b)
  -> m a
  -> m b
apply =
  error "todo"

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
--
-- | Apply a binary function in the Moonad environment.
--
-- >>> lift2 (+) (Id 7) (Id 8)
-- Id 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum [4,5,6]
-- 18
lift2 ::
  Moonad m =>
  (a -> b -> c)
  -> m a
  -> m b
  -> m c
lift2 =
  error "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
--
-- | Apply a ternary function in the Moonad environment.
--
-- >>> lift3 (\a b c -> a + b + c) (Id 7) (Id 8) (Id 9)
-- Id 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product [4,5,6]
-- 138
lift3 ::
  Moonad m =>
  (a -> b -> c -> d)
  -> m a
  -> m b
  -> m c
  -> m d
lift3 =
  error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
--
-- | Apply a quaternary function in the Moonad environment.
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Id 7) (Id 8) (Id 9) (Id 10)
-- Id 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) [4,5,6]
-- 148
lift4 ::
  Moonad m =>
  (a -> b -> c -> d -> e)
  -> m a
  -> m b
  -> m c
  -> m d
  -> m e
lift4 =
  error "todo"

-- Exercise 17
-- Relative Difficulty: 3
--
-- | Sequences a list of structures to a structure of list.
--
-- >>> seequence [Id 7, Id 8, Id 9]
-- Id [7,8,9]
--
-- >>> seequence [1 :. 2 :. 3 :. Nil, 1 :. 2 :. Nil]
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> seequence [Full 7, Empty]
-- Empty
--
-- >>> seequence [Full 7, Full 8]
-- Full [7,8]
--
-- >>> seequence [(*10), (+2)] 6
-- [60,8]
seequence ::
  Moonad m =>
  [m a]
  -> m [a]
seequence =
  error "todo"

-- Exercise 18
-- Relative Difficulty: 3
--
-- | Traverse (map) a list of values with an effect.
--
-- >>> traaverse (\n -> Id (n + 4)) [1,2,3]
-- Id [5,6,7]
--
-- >>> traaverse (\n -> n :. n * 2 :. Nil) [1,2,3]
-- [[1,2,3],[1,2,6],[1,4,3],[1,4,6],[2,2,3],[2,2,6],[2,4,3],[2,4,6]]
--
-- >>> traaverse (\n -> if n < 7 then Full (n * 3) else Empty) [1,2,3]
-- Full [3,6,9]
--
-- >>> traaverse (\n -> if n < 7 then Full (n * 3) else Empty) [1,2,3,14]
-- Empty
--
-- >>> traaverse (*) [1,2,3] 15
-- [15,30,45]
traaverse ::
  Moonad m =>
  (a -> m b)
  -> [a]
  -> m [b]
traaverse =
  error "todo"

-- Exercise 19
-- Relative Difficulty: 4
--
-- | Replicate an effect a given number of times.
--
-- >>> reeplicate 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> reeplicate 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> reeplicate 4 Empty
-- Empty
--
-- >>> reeplicate 4 (*2) 5
-- [10,10,10,10]
reeplicate ::
  Moonad m =>
  Int
  -> m a
  -> m [a]
reeplicate =
  error "todo"

-- Exercise 20
-- Relative Difficulty: 9
--
-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (Id . even) [4,5,6]
-- Id [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else if a > 7 then Full False else Full True) [4,5,6]
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else if a > 7 then Full False else Full True) [4,5,6,7,8,9]
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else if a > 7 then Full False else Full True) [4,5,6,13,14]
-- Empty
--
-- >>> filtering (>) [4..12] 8
-- [9,10,11,12]
filtering ::
  Moonad m =>
  (a -> m Bool)
  -> [a]
  -> m [a]
filtering =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
