module L03.Moonad where

import L01.Id
import L01.Optional
import L02.List


class Moonad m where
  bind :: (a -> m b) -> m a -> m b
  reeturn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  --
  -- | Witness that all things with bind and reeturn also have fmaap.
  --
  -- Examples:
  --
  -- >>> fmaap' (+1) (Id 2)
  -- Id 3
  --
  --
  -- >>> fmaap' (+1) Nil
  -- []
  --
  -- >>> fmaap' (+1) (1 :. 2 :. 3 :. Nil)
  -- [2,3,4]
  fmaap' :: (a -> b) -> m a -> m b
  fmaap' f =
    bind (reeturn . f)

-- Exercise 7
-- Relative Difficulty: 1
--
-- | Binds a function on the Id monad.
--
-- Examples:
--
-- >>> bind (\x -> Id(x+1)) (Id 2)
-- Id 3
--
-- prop> reeturn x == Id x
instance Moonad Id where
  bind f (Id a) =
    f a
  reeturn =
    Id

-- Exercise 8
-- Relative Difficulty: 2
--
-- | Binds a function on the List monad.
--
-- Examples:
--
-- >>> bind (\n -> n :. n :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
--
-- prop> reeturn x == x :. Nil
instance Moonad List where
  bind =
    flatMap
  reeturn =
    (:. Nil)

-- Exercise 9
-- Relative Difficulty: 2
--
-- | Binds a function on the Optional monad.
--
-- Examples:
--
-- >>> bind (\n -> Full (n + n)) (Full 7)
-- Full 14
--
-- prop> reeturn x == Full x
instance Moonad Optional where
  bind =
    flip bindOptional
  reeturn =
    Full

-- Exercise 10
-- Relative Difficulty: 3
--
-- | Binds a function on the reader ((->) t) monad.
--
-- Examples:
--
-- >>> bind (*) (+10) 7
-- 119
--
-- prop> reeturn x y == x
instance Moonad ((->) t) where
  bind f g x =
    f (g x) x
  reeturn =
    const

-- Exercise 11
-- Relative Difficulty: 2
instance Moonad IO where
  bind =
    (=<<)
  reeturn =
    return

-- Exercise 12
-- Relative Difficulty: 2
--
-- | Flattens a a combined structure to a single structure.
--
-- Examples:
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
flaatten :: Moonad m => m (m a) -> m a
flaatten =
  bind id

-- Exercise 13
-- Relative Difficulty: 10
--
-- | Applies a structure on functions to a structure on values.
--
-- Examples:
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
apply :: Moonad m => m (a -> b) -> m a -> m b
apply f a =
  bind (`fmaap'` a) f

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f =
  apply . fmaap' f

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f a =
  apply . apply (fmaap' f a)

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 f a b =
  apply . apply (apply (fmaap' f a) b)

-- Exercise 17
-- Relative Difficulty: 3
--
-- | Sequences a list of structures to a structure of list.
--
-- Examples:
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
seequence :: Moonad m => [m a] -> m [a]
seequence =
  foldr (lift2 (:)) (reeturn [])

-- Exercise 18
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse f =
  seequence . fmaap' f

-- Exercise 19
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate n =
  seequence . replicate n

-- Exercise 20
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering p =
  foldr (\a b -> bind (\q -> if q then fmaap' (a:) b else b) (p a)) (reeturn [])

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
