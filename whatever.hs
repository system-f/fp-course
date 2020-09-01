{-
x :: Integer
x = 99

f1 :: Integer -> Integer
f1 v = v + 10

f1' :: Integer -> Integer
f1' = \v -> v + 10

f2 :: Integer -> (Integer -> Integer)
f2 v w = (v + w) * 2

f2' = \v w -> (v + w) * 2

f2'' v = \w -> (v + w) * 2

f2''' = \v -> \w -> (v + w) * 2

-- All Haskell functions, take one argument, always, no exceptions
-- "takes two arguments"
-- if you start believing it, then we go back

(.+.) :: Integer -> Integer -> Integer
(.+.) v w = (v + w) * 2

-- "once-inhabited type"
blah :: p1 -> p2 -> p2
blah = \a b -> b

blah' :: (b -> c) -> (a -> b) -> a -> c
blah' = \dsfbg -> \dfg -> \rftgyhj -> dsfbg (dfg rftgyhj)

-- if nothing else, type underscore
-- if you ever need a function:
--   1. \
--   2. identifier name
--   3. ->
--   4. _
--   5. :reload

data Shape {- 0 or more type variables -} = Circle Integer | Rectangle Integer Integer | Triangle Integer Integer Integer

pie = 3

perimeter :: Shape -> Integer
perimeter = \s -> case s of
                    Circle r -> r * 2 * pie
                    Rectangle w h -> (w + h) * 2
                    Triangle a b c -> a + b + c

perimeter' :: Shape -> Integer
perimeter' (Circle r) = r * 2 * pie
perimeter' (Rectangle w h) = (w + h) * 2
perimeter' (Triangle a b c) = a + b + c

data Three a = Three a a a
  deriving (Eq, Show)
-- 9
mapThree :: (a -> b) -> Three a -> Three b
mapThree = \f -> \x -> case x of
                         Three a1 a2 a3 -> Three (f a1) (f a2) (f a3)

-- eta-reduction
-- \x -> f x
-- f
-}
