
-- asfsaf
add :: Int -> (Int -> Int)
add x y = x + y


add' :: Int -> Int -> Int
add' x = \y -> x + y


add'' :: Int -> Int -> Int
add'' = \x -> \y -> x + y


add''' :: Int -> Int -> Int
add''' = \x y -> x + y

hof :: (Int -> Int) -> Int
hof f = f 3

data Shape = 
  Circle Int
  | Square Int
  | Rectangle Int Int
  deriving (Eq, Show)

perim :: Shape -> Int
perim (Circle r) = r * 2 * 3
perim (Rectangle w h) = w * 2 + h * 2
perim (Square s) = s * 4

perim' :: Shape -> Int
perim' s = 
  case s of
    Circle r -> undefined
    Rectangle w h -> undefined
    Square s -> s

first :: a -> a -> a
first x _ = x

data Container a =
  C a

blab :: (a -> b) -> Container a -> b
--       f :: a -> b
--       (C x) :: Container a
--       x :: a
--       f x :: b
blab f (C x) = f x


blab' :: (a -> b) -> Container a -> Container b
blab' f (C x) = C (f x)

data Fred a = 
  F a
  | a :? a
  deriving (Eq, Show)

