{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo") with an appropriate solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

module Course.List where

import Course.Core
import Course.Optional
import qualified Prelude as P
import qualified Numeric as N


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
data List t = Nil | t :. List t deriving Eq

-- Right-associative
infixr 5 :.

instance Show t => Show (List t) where
  show = show . foldRight (:) []

-- The list of integers from zero to infinity.
infinity ::
  List Integer
infinity =
  let inf x = x :. inf (x+1)
  in inf 0

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

-- END Helper functions and data types

-- BEGIN Exercises

-- Exercise 1
--
-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> x `headOr` infinity == 0
--
-- prop> x `headOr` Nil == x
headOr ::
  a
  -> List a
  -> a
headOr =
  error "todo"

-- | The product of the elements of a list.
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
product ::
  List Int
  -> Int
product =
  foldLeft (*) 1

-- Exercise 2
--
-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- prop> foldLeft (-) (sum x) x == 0
sum ::
  List Int
  -> Int
sum =
  error "todo"

-- Exercise 3
--
-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- prop> sum (map (const 1) x) == length x
length ::
  List a
  -> Int
length =
  error "todo"

-- Exercise 4
--
-- | Map the given function on each element of the list.
--
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> headOr x (map (+1) infinity) == 1
--
-- prop> map id x == x
map ::
  (a -> b)
  -> List a
  -> List b
map =
  error "todo"

-- Exercise 5
--
-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> headOr x (filter (const True) infinity) == 0
--
-- prop> filter (const True) x == x
--
-- prop> filter (const False) x == Nil
filter ::
  (a -> Bool)
  -> List a
  -> List a
filter =
  error "todo"

-- Exercise 6
--
-- | Append two lists to a new list.
--
-- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> headOr x (Nil ++ infinity) == 0
--
-- prop> headOr x (y ++ infinity) == headOr 0 y
--
-- prop> (x ++ y) ++ z == x ++ (y ++ z)
--
-- prop> x ++ Nil == x
(++) ::
  List a
  -> List a
  -> List a
(++) =
  error "todo"

-- Exercise 7
--
-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> sum (map length x) == length (flatten x)
flatten ::
  List (List a)
  -> List a
flatten =
  error "todo"

-- Exercise 8
--
-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> flatMap id (x :: List (List Int)) == flatten x
flatMap ::
  (a -> List b)
  -> List a
  -> List b
flatMap =
  error "todo"

-- Exercise 9
--
-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values, 
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- * The only time `Empty` is returned is
-- when the list contains one or more `Empty` values.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty
seqOptional ::
  List (Optional a)
  -> Optional (List a)
seqOptional =
  error "todo"

-- Exercise 10
--
-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
find ::
  (a -> Bool)
  -> List a
  -> Optional a
find =
  error "todo"

-- Exercise 11
--
-- | Reverse a list.
--
-- >>> reverse (1 :. 2 :. 3 :. Nil)
-- [3,2,1]
--
-- prop> let types = x :: List Int in (reverse . reverse) x == x
--
-- prop> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse ::
  List a
  -> List a
reverse =
  error "todo"

-- END Exercises

hlist ::
  List a
  -> [a]
hlist =
  foldRight (:) []

listh ::
  [a]
  -> List a
listh =
  P.foldr (:.) Nil

putStr ::
  List Char
  -> IO ()
putStr =
  P.putStr . hlist

putStrLn ::
  List Char
  -> IO ()
putStrLn =
  P.putStrLn . hlist

readFile ::
  Filename
  -> IO Str
readFile =
  P.fmap listh . P.readFile . hlist

writeFile ::
  Filename
  -> Str
  -> IO ()
writeFile n s =
  P.writeFile (hlist n) (hlist s)

isPrefixOf ::
  Eq a =>
  List a
  -> List a
  -> Bool
isPrefixOf Nil _ =
  True
isPrefixOf _  Nil =
  False
isPrefixOf (x:.xs) (y:.ys) =
  x == y && isPrefixOf xs ys

isEmpty ::
  List a
  -> Bool
isEmpty Nil =
  True
isEmpty (_:._) =
  False

zip ::
  List a
  -> List b
  -> List (a, b)
zip =
  zipWith (,)

zipWith ::
  (a -> b -> c)
  -> List a
  -> List b
  -> List c
zipWith f (a:.as) (b:.bs) =
  f a b :. zipWith f as bs
zipWith _ _  _ =
  Nil

unfoldr ::
  (a -> Optional (b, a))
  -> a
  -> List b
unfoldr f b  =
  case f b of
    Full (a, z) -> a :. unfoldr f z
    Empty -> Nil

any ::
  (a -> Bool)
  -> List a
  -> Bool
any p =
  foldRight ((||) . p) False

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight ((&&) . p) True

or ::
  List Bool
  -> Bool
or =
  any id

and ::
  List Bool
  -> Bool
and =
  all id

elem ::
  Eq a =>
  a
  -> List a
  -> Bool
elem x =
  any (== x)

notElem ::
  Eq a =>
  a
  -> List a
  -> Bool
notElem x =
  all (/= x)

permutations
  :: List a -> List (List a)
permutations xs0 =
  let perms Nil _ =
        Nil
      perms (t:.ts) is =
        let interleave' _ Nil r =
              (ts, r)
            interleave' f (y:.ys) r =
               let (us,zs) = interleave' (f . (y:.)) ys r
               in  (y:.us, f (t:.y:.us):.zs)
        in foldRight (\xs -> snd . interleave' id xs) (perms ts (t:.is)) (permutations is)
  in xs0 :. perms xs0 Nil

intersectBy ::
  (a -> b -> Bool)
  -> List a
  -> List b
  -> List a
intersectBy e xs ys =
  filter (\x -> any (e x) ys) xs

take ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
take n _  | n <= 0 =
  Nil
take _ Nil =
  Nil
take n (x:.xs) =
  x :. take (n - 1) xs

drop ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
drop n xs | n <= 0 =
  xs
drop _ Nil =
  Nil
drop n (_:.xs) =
  drop (n-1) xs

repeat ::
  a
  -> List a
repeat x =
  x :. repeat x

replicate ::
  (Num n, Ord n) =>
  n
  -> a
  -> List a
replicate n x =
  take n (repeat x)

reads ::
  P.Read a =>
  Str
  -> Optional (a, Str)
reads s =
  case P.reads (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

read ::
  P.Read a =>
  Str
  -> Optional a
read =
  mapOptional fst . reads

readHexs ::
  (Eq a, Num a) =>
  Str
  -> Optional (a, Str)
readHexs s =
  case N.readHex (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readHex ::
  (Eq a, Num a) =>
  Str
  -> Optional a
readHex =
  mapOptional fst . readHexs

readFloats ::
  (RealFrac a) =>
  Str
  -> Optional (a, Str)
readFloats s =
  case N.readSigned N.readFloat (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readFloat ::
  (RealFrac a) =>
  Str
  -> Optional a
readFloat =
  mapOptional fst . readFloats

instance IsString (List Char) where
  fromString =
    listh

type Str =
  List Char

type Filename =
  Str

strconcat ::
  [Str]
  -> P.String
strconcat =
  P.concatMap hlist

stringconcat ::
  [P.String]
  -> P.String
stringconcat =
  P.concat

instance P.Monad List where
  (>>=) =
    flip flatMap
  return =
    (:. Nil)
