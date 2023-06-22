module Course.Scratch where

import Prelude

x :: Integer
x = 99

-- -> is right-associative
f1 :: Integer -> Integer
f1 v = v + 10

-- how many arguments does f2 take?
-- 1
-- All functions in Haskell, always, take 1 argument
f2 :: Integer -> (Integer -> Integer)
f2 v w = (v + w) * 2

f2' :: Integer -> Integer -> Integer
f2' v = \w -> (v + w) * 2

f2'' :: Integer -> Integer -> Integer
f2'' = \v -> \w -> (v + w) * 2

f3 :: (Integer -> x) -> x
f3 k = k 99

-- once-inhabited type
f4 :: a -> b -> b
f4 = \_ b -> b

-- once-inhabited type
-- "function composition"
f5 :: (b -> c) -> (a -> b) -> a -> c
f5 = (.)

-- "type-hole driven development"
-- stuck? type underscore, then :reload
--   look at "Found hole", "Relevant bindings" and "Valid holes"
--   if "Found hole" is a function, type \name -> _ then :reload
--   repeat
--
--  Valid holes = answers the fit in the hole
-- Relevant bindings = things that are relevant to solving the problem
(.+.) :: Integer -> Integer -> Integer
(.+.) v w = (v + w) * 2

data Shape = Circle Integer | Rectangle Integer Integer | Triangle Integer Integer Integer
  deriving Show

perimeter :: Shape -> Integer
perimeter (Circle r) = r * 3 * 2
perimeter (Rectangle w h) = (w + h) * 2
perimeter (Triangle a b c) = a + b + c

-- Java
-- interface TwoOrThree<a> {}
-- class Twoo<a> extends TwoOrThree<a> { a a1; a a2; }
-- class Threee<a> extends TwoOrThree<a> { a a1; a a2; a a3; }
data TwoOrThree a = Twoo a a | Threee a a a
  deriving Show

mapTwoOrThree :: (a -> b) -> TwoOrThree a -> TwoOrThree b
mapTwoOrThree f (Twoo a1 a2) = Twoo (f a1) (f a2)
mapTwoOrThree f (Threee a1 a2 a3) = Threee (f a1) (f a2) (f a3)

mapTwoOrThree' :: (a -> b) -> TwoOrThree a -> TwoOrThree b
mapTwoOrThree' = \f -> \t -> case t of
  Twoo a1 a2 -> Twoo (f a1) (f a2)
  Threee a1 a2 a3 -> Threee (f a1) (f a2) (f a3)
