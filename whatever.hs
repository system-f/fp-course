x :: Integer
x = 99

-- Integer a(Integer b)
a :: Integer -> Integer
a b = b + 10

f :: Integer -> (Integer -> Integer)
f i j = (i + j) * 2

ff :: Integer -> Integer -> Integer
ff = \i j -> (i + j) * 2

fff :: Integer -> Integer -> Integer
fff i = \j -> (i + j) * 2

(+++) :: Integer -> Integer -> Integer
(+++) i = \j -> (i + j) * 2

data TwoIntegers = TwoIntegers' Integer Integer
  deriving (Eq, Show)

-- many forms
-- polymorphism

-- class TwoAnything<a> { 
data TwoAnything a = TwoAnything' a a
  deriving (Eq, Show)

g :: TwoAnything Integer
g = TwoAnything' 66 77

data Shape =
  Square Integer |
  Rectangle Integer Integer |
  Triangle Integer Integer Integer |
  Circle Integer
  deriving (Eq, Show)

pie = 3

perimeter :: Shape -> Integer
perimeter (Square x) = x * 4
perimeter (Rectangle x y) = (x + y) * 2
perimeter (Triangle x y z) = x + y + z
perimeter (Circle r) = 45

perimeteragain :: Shape -> Integer
perimeteragain = \s -> case s of
  Square x -> x * 4
  Rectangle x y -> (x + y) * 2
  Triangle x y z -> x + y + z
  Circle r -> r * pie * 2



m :: (a -> b) -> TwoAnything a -> TwoAnything b
m = \a2b -> \fsht -> case fsht of
  TwoAnything' a1 a2 -> TwoAnything' (a2b a1) (a2b a2)

u :: a -> b -> a
u = \a -> \_ -> a

-- v :: Integer -> b -> Integer


