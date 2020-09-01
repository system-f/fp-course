x :: Integer
x = 99

f1 :: Integer -> Integer
f1 v = v + 10

f1' :: Integer -> Integer
f1' = \v -> v + 10

f2 :: Integer -> (Integer -> Integer)
f2 v w = (v + w) * 2

f2' v = \w -> (v + w) * 2

f2'' = \v -> \w -> (v + w) * 2

(.+.) v w = (v + w) * 2

-- "once-inhabited types"
blah :: a -> b -> b
blah = \a b -> b

-- 1. if "don't know", type underscore (then :reload, then read the error)
-- 2. if you ever need a function, type "\name -> _" (then :reload, then read the error)
blah' :: (b -> c) -> (a -> b) -> a -> c
blah' = \f -> \g -> \a -> f (g a)

data Shape = Circle Integer | Rectangle Integer Integer | Triangle Integer Integer Integer
  deriving (Eq, Show)

perimeter :: Shape -> Integer
perimeter = \s -> case s of
  Circle r -> r * 3
  Rectangle w h -> (w + h) * 2
  Triangle a b c -> a + b + c

perimeter' :: Shape -> Integer
perimeter' (Circle r) = r * 3
perimeter' (Rectangle w h) = (w + h) * 2
perimeter' (Triangle a b c) = a + b + c

data TwoOrThree a = Two a a | Three a a a
  deriving (Eq, Show)

mapThree :: (a -> b) -> TwoOrThree a -> TwoOrThree b
mapThree = \a2b t -> case t of
  Two a1 a2 -> Two (a2b a1) (a2b a2)
  Three a1 a2 a3 -> Three (a2b a1) (a2b a2) (a2b a3)

-- eta-reduction
-- \x -> f x
-- f

-- composition
-- \x -> f (g x)
-- f . g

