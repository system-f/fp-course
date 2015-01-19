{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified System.Environment as E
import qualified Prelude as P
import qualified Numeric as N


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
data List t =
  Nil
  | t :. List t
  deriving (Eq, Ord)

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
  foldRight const

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
  foldLeft (+) 0

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
  foldLeft (const . succ) 0

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
map f =
  foldRight (\a b -> f a :. b) Nil

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
filter f =
  foldRight (\a -> if f a then (a:.) else id) Nil

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
  flip (foldRight (:.))

infixr 5 ++

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
  foldRight (++) Nil

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
flatMap f =
  flatten . map f

-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> let types = x :: List (List Int) in flatten x == flattenAgain x
flattenAgain ::
  List (List a)
  -> List a
flattenAgain =
  flatMap id

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
  foldRight (twiceOptional (:.)) (Full Nil)

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
find p x =
  case filter p x of
    Nil -> Empty
    h:._ -> Full h

-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
lengthGT4 ::
  List a
  -> Bool
lengthGT4 (_:._:._:._:._:._) =
  True
lengthGT4 _ =
  False

-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- >>> take 1 (reverse (reverse largeList))
-- [1]
--
-- prop> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse ::
  List a
  -> List a
reverse =
  foldLeft (flip (:.)) Nil

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce ::
  (a -> a)
  -> a
  -> List a
produce f a =
  a :. produce f (f a)

-- | Do anything other than reverse a list.
-- Is it even possible?
--
-- >>> notReverse Nil
-- []
--
-- prop> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)
--
-- prop> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
notReverse ::
  List a
  -> List a
notReverse =
  reverse -- impossible
-- For the sake of discussion, let's assume that,
-- xs, ys :: List a
-- x, y :: a
-- We are given the following properties
-- notReverse xs ++ notReverse ys = notReverse (ys ++ xs)  -- Eq. 1
-- notReverse (x :. Nil) = x :. Nil                        -- Eq. 2
--
-- Now, to define
--   notReverse Nil,
-- we, first, observe that
-- notReverse (x :. Nil) ++ notReverse Nil                 -- Eq. 3
--   = notReverse (Nil ++ (x :. Nil))                      -- using Eq. 1
--   = notReverse (x :. Nil)                               -- defn of (++)
--   = x :. Nil                                            -- using Eq. 2
--
-- And
-- notReverse (x :. Nil) ++ notReverse Nil                 -- Eq. 4
--   = (x :. Nil) ++ notReverse Nil                        -- using Eq. 2
--
-- Since, the LHS of Eq. 3 and the LHS of Eq. 4 are the same,
-- the RHS of Eq. 4 must be equal to the RHS of Eq. 3, i.e.,
-- (x :. Nil) ++ notReverse Nil = x :. Nil
--                              = (x :. Nil) ++ Nil        -- defn of (++)
-- => notReverse Nil = Nil                                 -- Eq. 5
--
-- Next, we see that
-- notReverse (x :. Nil) ++ notReverse (y :. Nil)          -- Eq. 6
--   = notReverse ((y :. Nil) ++ (x :. Nil))               -- using Eq. 1
--   = notReverse (y :. x :. Nil)                          -- defn of (++)
-- And that
-- notReverse (x :. Nil) ++ notReverse (y :. Nil)          -- Eq. 7
--   = (x :. Nil) ++ (y :. Nil)                            -- using Eq. 2
--   = x :. y :. Nil                                       -- defn of (++)
--
-- Again, since the LHS of both Eq. 6 and Eq 7. are the same,
-- the RHS of Eq. 6 and 7 must be the same, i.e.,
-- notReverse (y :. x :. Nil) = x :. y :. Nil              -- Eq. 8
-- Or, simply (using variable renaming), we get
-- notReverse (x :. y :. Nil) = y :. x :. Nil              -- Eq. 8'
--
-- Now, we want to prove that Eq. 1 and Eq. 2 imply 'reverse', using
-- mathematical induction.
--
-- We note that
-- notReverse Nil = Nil = reverse Nil                      -- using Eq. 5, defn of reverse
-- notReverse (x :. Nil) = (x :. Nil) = reverse (x :. Nil) -- using Eq. 2, defn of reverse
-- notReverse (x :. y :. Nil) = (y :. x :. Nil)            -- using Eq. 1
--                            = reverse (x :. y :. Nil)    -- defn of reverse
-- i.e., notReverse is equal to reverse for cases when 'List a' has 0,
-- 1, and 2 elements.
--
-- Assume that it is true when 'List a' has n (n > 0) elements,
-- notReverse (x1 :. x2 :. ... :. xn :. Nil)               -- Eq. 9
--   = (xn :. x{n-1} :. ... :. x1 :. Nil)
--   = reverse (x1 :. x2 :. ... :. xn :. Nil)              -- defn of reverse
--
-- Now,
-- notReverse (x1 :. x2 :. ... :. xn :. x{n+1} :. Nil)
--   = notReverse ((x1 :. x2 :. ... :. xn :. Nil) ++ (x{n+1} :. Nil))  -- defn of (++)
--   = notReverse (x{n+1} :. Nil) ++ notReverse ((x1 :. x2 :. ... :. xn :. Nil)  -- using Eq. 1
--   = (x{n+1} :. Nil) ++ (xn :. x{n-1} :. ... :. x1 :. Nil)  -- using Eq. 2, Eq. 9 (assumption)
--   = (x{n+1} :. xn :. ... :. x1 :. Nil)                  -- defn of (++)
--   = reverse (x1 :. x2 :. ... :. xn :. x{n+1} :. Nil)    -- defn of reverse
--
-- Since,
--   - notReverse is equal to reverse for cases when 'List a' has 0, 1, and 2
--     elements, and
--   - if we assume that it is true when 'List a' has n (for some n > 0)
--     elements, it is also true when 'List a' has n+1 elements,
-- by mathematical induction notReverse is equal to reverse for all n in [0 ..].

largeList ::
  List Int
largeList =
  listh [1..50000]

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
  Chars
  -> IO ()
putStr =
  P.putStr . hlist

putStrLn ::
  Chars
  -> IO ()
putStrLn =
  P.putStrLn . hlist

readFile ::
  Filename
  -> IO Chars
readFile =
  P.fmap listh . P.readFile . hlist

writeFile ::
  Filename
  -> Chars
  -> IO ()
writeFile n s =
  P.writeFile (hlist n) (hlist s)

getLine ::
  IO Chars
getLine =
  P.fmap listh P.getLine

getArgs ::
  IO (List Chars)
getArgs =
  P.fmap (listh . P.fmap listh) E.getArgs

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

span ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
span p x =
  (takeWhile p x, dropWhile p x)

break ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
break p =
  span (not . p)

dropWhile ::
  (a -> Bool)
  -> List a
  -> List a
dropWhile _ Nil =
  Nil
dropWhile p xs@(x:.xs') =
  if p x
    then
      dropWhile p xs'
    else
      xs

takeWhile ::
  (a -> Bool)
  -> List a
  -> List a
takeWhile _ Nil =
  Nil
takeWhile p (x:.xs) =
  if p x
    then
      x :. takeWhile p xs
    else
      Nil

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

lines ::
  Chars
  -> List Chars
lines =
  listh . P.fmap listh . P.lines . hlist

unlines ::
  List Chars
  -> Chars
unlines =
  listh . P.unlines . hlist . map hlist

words ::
  Chars
  -> List Chars
words =
  listh . P.fmap listh . P.words . hlist

unwords ::
  List Chars
  -> Chars
unwords =
  listh . P.unwords . hlist . map hlist

listOptional ::
  (a -> Optional b)
  -> List a
  -> List b
listOptional _ Nil =
  Nil
listOptional f (h:.t) =
  let r = listOptional f t
  in case f h of
       Empty -> r
       Full q -> q :. r

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
  Chars
  -> Optional (a, Chars)
reads s =
  case P.reads (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

read ::
  P.Read a =>
  Chars
  -> Optional a
read =
  mapOptional fst . reads

readHexs ::
  (Eq a, Num a) =>
  Chars
  -> Optional (a, Chars)
readHexs s =
  case N.readHex (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readHex ::
  (Eq a, Num a) =>
  Chars
  -> Optional a
readHex =
  mapOptional fst . readHexs

readFloats ::
  (RealFrac a) =>
  Chars
  -> Optional (a, Chars)
readFloats s =
  case N.readSigned N.readFloat (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readFloat ::
  (RealFrac a) =>
  Chars
  -> Optional a
readFloat =
  mapOptional fst . readFloats

instance IsString (List Char) where
  fromString =
    listh

type Chars =
  List Char

type Filename =
  Chars

strconcat ::
  [Chars]
  -> P.String
strconcat =
  P.concatMap hlist

stringconcat ::
  [P.String]
  -> P.String
stringconcat =
  P.concat

show' ::
  Show a =>
  a
  -> List Char
show' =
  listh . show

instance P.Monad List where
  (>>=) =
    flip flatMap
  return =
    (:. Nil)
