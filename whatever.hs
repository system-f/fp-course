x :: Integer
x = 99

f :: Integer -> Integer
f n = n + 10

g :: Integer -> Integer
g = \c -> c + 20


why = h 90

h :: Integer -> (Integer -> Integer)
h    x          y =        (x + y) * 2

hagain :: Integer -> Integer -> Integer
hagain = \x y -> (x + y) * 2

handagain :: Integer -> Integer -> Integer
handagain x = \y -> (x + y) * 2

i :: (Integer -> whatever) -> whatever
i k = k 99

pp = i h x

-- most ridiculous
-- ...
-- most satisfactory
hithere :: x -> x
hithere a = a

byethere :: x -> x -> x
byethere a _ = a
-- byethere _ a = a

data Shape = Rectangle Integer Integer | Circle Integer | Triangle Integer Integer Integer
  deriving Show

pie = 3

perimeter :: Shape -> Integer
perimeter = \s -> case s of Rectangle w h -> (w + h) * 2
                            Circle r -> r * 2 * pie
                            Triangle a b c -> a + b + c


perimeteragain :: Shape -> Integer
perimeteragain (Rectangle w h) = (w + h) * 2
perimeteragain (Circle r) = r * 2 * pie
perimeteragain (Triangle a b c) = a + b + c


somethingelse :: Shape -> Integer
somethingelse (Rectangle w h) = (w + h) * 2
-- somethingelse _ = 0
somethingelse (Circle _) = 0
somethingelse (Triangle _ _ _) = 0

data Three a = T a a a
  deriving (Eq, Show)


mapThree :: (a -> b) -> Three a -> Three b
mapThree     f          (T a1 a2 a3) = T (f a1) (f a2) (f a3)














