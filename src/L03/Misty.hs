module L03.Misty where

import L01.Id
import L01.Optional
import L02.List


class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use banana and unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 5
-- Relative Difficulty: 1
instance Misty Id where
  banana f (Id a) = f a
  unicorn = Id

-- Exercise 6
-- Relative Difficulty: 2
instance Misty List where
  banana _ Nil    = Nil
  banana f (h:|t) = f h `append` banana f t
  unicorn = flip (:|) Nil

-- Exercise 7
-- Relative Difficulty: 2
instance Misty Optional where
  banana _ Empty    = Empty
  banana f (Full a) = f a
  unicorn = Full

-- Exercise 8
-- Relative Difficulty: 3
instance Misty ((->) t) where
  banana f g = \x -> f (g x) x
  unicorn x = \_ -> x

-- Exercise 9
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a
jellybean = banana id

-- Exercise 10
-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b
apple f a = (\f' -> (\a' -> f' a') `furry'` a) `banana` f

-- Exercise 11
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c
lemon2 f = apple . furry' f

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lemon3 f a = apple . apple (furry' f a)

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apple + lemon3)
lemon4 :: Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lemon4 f a b = apple . apple (apple (furry' f a) b)

-- Exercise 14
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
sausage []    = unicorn []
sausage (h:t) = (\h' -> (\t' -> h':t') `furry'` sausage t) `banana` h

-- Exercise 15
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
moppy f a = sausage (furry' f a)

-- Exercise 16
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar n a = sausage (replicate n a)

-- Exercise 17
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
filtering _ []    = unicorn []
filtering f (h:t) = (\g -> (if g then (h:) else id) `furry'` filtering f t) `banana` f h

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Misty [] where
  banana = concatMap
  unicorn = return
