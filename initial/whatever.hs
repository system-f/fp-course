x :: Integer
x = 88

y = 99

f :: Integer -> Integer
f z = z + 10

g :: (String -> a) -> a
g k = k "abc"

h :: Integer -> (Integer -> Integer)
h a b = (a + b) * 2

i :: (Integer -> a) -> a
i k = k 66

hh :: Integer -> Integer -> Integer
hh = \a -> \b -> (a + b) * 2

-- call the function (i) and pass the function, which adds 5
j :: Integer
j = i (\n -> n + 5)

-- I'm not even going to tell you.
-- "make it compile, anything"
k :: a -> b -> a
-- k = \v -> \_ -> v
k = \v _ -> v

-- data <data-type-name> (0 or more type variables) =
-- list of constructors, followed their arguments, separated by |
data Shape =
  Rectangle Integer Integer | Square Integer | Triangle Integer Integer Integer
  deriving (Eq, Show)

myTriangle :: Shape
myTriangle = Triangle 3 4 5

-- pattern-matching, case-analysis
perimeter :: Shape -> Integer
perimeter = \s -> case s of
  Rectangle w h -> (w + h) * 2
  Square x -> x * 4
  Triangle a b c -> a + b + c

perimeterAgain :: Shape -> Integer
perimeterAgain (Rectangle 33 h) = 77
-- perimeterAgain (Rectangle w h) = if w == 33 then 77 else (w + h) * 2
perimeterAgain (Square x) = x * 4
perimeterAgain (Triangle a b c) = a + b + c
-- perimeterAgain _ = 99




