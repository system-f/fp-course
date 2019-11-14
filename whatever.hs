module MyFirstHaskell where
  
x = 99

f :: Int -> Int
f a = a * 2

g :: Int -> (Int -> Int)
g a b = (a + b) * 2

(.+.) :: Int -> (Int -> Int)
(.+.) a b = (a + b) * 2

-- h :: Int -> something -> something
h :: (Int -> something) -> something
h q = q 55

h1 = h

hh = h (\e -> e * 5)
hhh = h (* 5)

i :: something -> somethingelse -> something
i = \a -> \b -> a

data Shape =
  Circle Int | Rectangle Int Int | Triangle Int Int Int
  deriving (Eq, Show)

pie = 3
perimeter :: Shape -> Int
perimeter = \s -> case s of
                    Circle r -> r * 2 * pie
                    Rectangle w h -> (w + h) * 2
                    -- _ -> 77
                    -- Triangle a b c -> a + b + c

perimeteragain :: Shape -> Int
perimeteragain (Circle r) = r * 2 * pie
perimeteragain (Rectangle w h) = (w + h) * 2
perimeteragain (Triangle a b c) = a + b + c

data Three a = T a a a
  deriving (Eq, Show)

first :: Three a -> a
first = \t -> case t of
                T a1 _ _ -> a1

add3 :: Three Int -> Int
add3 = \t -> case t of
               T a1 a2 a3 -> a1 + a2 + a3

mapThree :: (a -> b) -> Three a -> Three b
mapThree = \f -> \(T a1 a2 a3) -> T (f a1) (f a2) (f a3)
-- first (T a1 _ _) = a1

data Two a = (:+) a a
  deriving Show

areTheseEqual :: (a -> a -> Bool) -> Two a -> Two a -> Bool
areTheseEqual f (a1 :+ a2) (b1 :+ b2) = 
  f a1 b1 && f a2 b2

areTheseEqual' :: Equal a => Two a -> Two a -> Bool
areTheseEqual' (a1 :+ a2) (b1 :+ b2) = 
  a1 .==. b1 && a2 .==. b2

-- hey guys, all your instances better be symmetric, or else
class Equal a where
  (.==.) :: a -> a -> Bool

instance Equal Bool where
  True .==. True = True
  False .==. False = True
  _ .==. _ = False

instance Equal Int where
  (.==.) = (==)

instance Equal a => Equal (Two a) where
  (a1 :+ a2) .==. (b1 :+ b2) =
    a1 .==. b1 && a2 .==. b2

-- i = print "1" >>  print "2" >> return (\a b -> a)










