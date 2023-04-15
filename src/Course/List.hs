{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Course.List
--
-- + Complete the exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo: ...") with an appropriate
--   solution.
--
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
--
-- + Bonus for using the provided functions, `foldRight` and `foldLeft`,
--   or for using one of your solutions to help solve another.
module Course.List where

import Control.Applicative qualified as A
import Control.Monad qualified as M
import Course.Core
import Course.Core.Prelude
import Numeric qualified as N
import System.Environment qualified as E
import Prelude (Maybe (..), String)
import Prelude qualified as P

type List :: Type -> Type

-- | A list of values that are all the same type.
--
-- 'List' has two data constructors:
--
-- > Nil  :: forall t.                List t
-- > (:.) :: forall t. t -> List t -> List t
--
-- So a list is either 'Nil', which has no fields,
-- or it's `(:.) x y`, where `x :: a` is the first element of the list
-- and `y :: List a` is the rest of the list.
--
-- When pattern matching, we can name the fields whatever we like,
-- so it's tradition to name them `(:.) x xs`,
-- in order to make it easy to remember that `x` is a list element
-- and `xs` is the rest of the list.
-- I suggest you follow this convention, too. Most Haskell code does.
-- But they're variable names, so they can be whatever makes sense to you.
--
-- In the code, we write `(:.)` in infix position,
-- but we could have written it in prefix position if we'd wanted to.
--
-- > data List t = Nil | (:.) t (List t)
--
-- The appeal of using infix position is that it requires fewer parentheses.
-- Haskellers tend to like fewer parentheses.
data List t = Nil | t :. List t
--                       ^^^^^^ type of second argument
--                  ^ type of first argument
--                    ^^ second data constructor. is symbolic, so infix.
--            ^^^ first data constructor, zero arguments
--        ^ type variable
--   ^^^^ type name
  deriving (Eq, Ord)
--              ^^^ compiler generates instance 'Ord a => Ord (List a)'
--          ^^ compiler generates instance 'Eq a => Eq (List a)'

infixr 5 :.
-- ^^^^^^^^ Fixity and precedence declaration for `:.`.
--          This is optional, the compiler has a default it will you if you don't write one.
--          But it's usually a good idea to write such a declaration exlicity for operators you define.
-- ^^^ `infixr` means `x :. y :. zs` is parsed as `x :. (y :. zs)`.
--     ^ precedence 5 means that this operation binds more tightly than operations with a lower precedence.
--       For example, when we define `(++)`, we'll give it a precedence of 4.
--       `x :. xs ++ ys` will be parsed as `(x :. xs) ++ ys`.

instance Show t => Show (List t) where
    show = show . hlist

-- | Question List 1
--
-- Our 'List' type is analogous to the '[]' type in Haskell's base library.
--
-- > data [a] = [] | a : [a]
-- > --                ^ "cons" list constructor
-- > --         ^^ empty list data constructor
--
-- Instead of @List a@, we have @[a]@.
-- Instead of @Nil@, we have @[]@.
-- Instead of @(:.)@, we have @(:)@.
question_List_1 :: String
question_List_1 = error "TODO"

-- The list of integers from zero to infinity.
infinity :: List Integer
infinity =
    let inf x = x :. inf (x + 1)
     in inf 0

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil = b
foldRight f b (h :. t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

-- END Helper functions and data types

{- | Returns the head of the list or the given default.

 >>> headOr 3 (1 :. 2 :. Nil)
 1

 >>> headOr 3 Nil
 3

 prop> \x -> x `headOr` infinity == 0

 prop> \x -> x `headOr` Nil == x
-}
headOr :: a -> List a -> a
headOr =
    error "todo: Course.List#headOr"

{- | The product of the elements of a list.

 >>> product Nil
 1

 >>> product (1 :. 2 :. 3 :. Nil)
 6

 >>> product (1 :. 2 :. 3 :. 4 :. Nil)
 24
-}
product :: List Int -> Int
product =
    error "todo: Course.List#product"

{- | Sum the elements of the list.

 >>> sum (1 :. 2 :. 3 :. Nil)
 6

 >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
 10

 prop> \x -> foldLeft (-) (sum x) x == 0
-}
sum :: List Int -> Int
sum =
    error "todo: Course.List#sum"

{- | Return the length of the list.

 >>> length (1 :. 2 :. 3 :. Nil)
 3

 prop> \x -> sum (map (const 1) x) == length x
-}
length :: List a -> Int
length =
    error "todo: Course.List#length"

{- | Map the given function on each element of the list.

 >>> map (+10) (1 :. 2 :. 3 :. Nil)
 [11,12,13]

 prop> \x -> headOr x (map (+1) infinity) == 1

 prop> \x -> map id x == x
-}
map :: (a -> b) -> List a -> List b
map =
    error "todo: Course.List#map"

{- | Return elements satisfying the given predicate.

 >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
 [2,4]

 prop> \x -> headOr x (filter (const True) infinity) == 0

 prop> \x -> filter (const True) x == x

 prop> \x -> filter (const False) x == Nil
-}
filter :: (a -> Bool) -> List a -> List a
filter =
    error "todo: Course.List#filter"

{- | Append two lists to a new list.

 >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
 [1,2,3,4,5,6]

 prop> \x -> headOr x (Nil ++ infinity) == 0

 prop> \x -> headOr x (y ++ infinity) == headOr 0 y

 prop> \x -> (x ++ y) ++ z == x ++ (y ++ z)

 prop> \x -> x ++ Nil == x
-}
(++) :: List a -> List a -> List a
(++) =
    error "todo: Course.List#(++)"

infixr 5 ++

{- | Flatten a list of lists to a list.

 >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
 [1,2,3,4,5,6,7,8,9]

 prop> \x -> headOr x (flatten (infinity :. y :. Nil)) == 0

 prop> \x -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y

 prop> \x -> sum (map length x) == length (flatten x)
-}
flatten :: List (List a) -> List a
flatten =
    error "todo: Course.List#flatten"

{- | Map a function then flatten to a list.

 >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
 [1,2,3,2,3,4,3,4,5]

 prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0

 prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y

 prop> \x -> flatMap id (x :: List (List Int)) == flatten x
-}
flatMap :: (a -> List b) -> List a -> List b
flatMap =
    error "todo: Course.List#flatMap"

{- | Flatten a list of lists to a list (again).
 HOWEVER, this time use the /flatMap/ function that you just wrote.

 prop> \x -> let types = x :: List (List Int) in flatten x == flattenAgain x
-}
flattenAgain :: List (List a) -> List a
flattenAgain =
    error "todo: Course.List#flattenAgain"

{- | Convert a list of optional values to an optional list of values.

 * If the list contains all `Just` values,
 then return `Just` list of values.

 * If the list contains one or more `Nothing` values,
 then return `Nothing`.

 >>> seqOptional (Just 1 :. Just 10 :. Nil)
 Just [1, 10]

 >>> seqOptional Nil
 Just []

 >>> seqOptional (Just 1 :. Just 10 :. Nothing :. Nil)
 Nothing

 >>> seqOptional (Nothing :. map Just infinity)
 Nothing
-}
seqOptional :: List (Maybe a) -> Maybe (List a)
seqOptional =
    error "todo: Course.List#seqOptional"

{- | Find the first element in the list matching the predicate.

 >>> find even (1 :. 3 :. 5 :. Nil)
 Nothing

 >>> find even Nil
 Nothing

 >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
 Just 2

 >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
 Just 2

 >>> find (const True) infinity
 Just 0
-}
find :: (a -> Bool) -> List a -> Maybe a
find =
    error "todo: Course.List#find"

{- | Determine if the length of the given list is greater than 4.

 >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
 False

 >>> lengthGT4 Nil
 False

 >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
 True

 >>> lengthGT4 infinity
 True
-}
lengthGT4 :: List a -> Bool
lengthGT4 =
    error "todo: Course.List#lengthGT4"

{- | Reverse a list.

 >>> reverse Nil
 []

 >>> take 1 (reverse (reverse largeList))
 [1]

 prop> \x -> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)

 prop> \x -> let types = x :: Int in reverse (x :. Nil) == x :. Nil
-}
reverse :: List a -> List a
reverse =
    error "todo: Course.List#reverse"

{- | Produce an infinite `List` that seeds with the given value at its head,
 then runs the given function for subsequent elements

 >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
 [0,1,2,3]

 >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
 [1,2,4,8]
-}
produce :: (a -> a) -> a -> List a
produce f x =
    error "todo: Course.List#produce"

{- | Do anything other than reverse a list.
 Is it even possible?

 >>> notReverse Nil
 []

 prop> \x y -> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)

 prop> \x -> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
-}
notReverse :: List a -> List a
notReverse =
    error "todo: Is it even possible?"

---- End of list exercises

largeList :: List Int
largeList =
    listh [1 .. 50000]

hlist :: List a -> [a]
hlist =
    foldRight (:) []

listh :: [a] -> List a
listh =
    P.foldr (:.) Nil

putStr :: Chars -> IO ()
putStr =
    P.putStr . hlist

putStrLn :: Chars -> IO ()
putStrLn =
    P.putStrLn . hlist

readFile :: FilePath -> IO Chars
readFile =
    P.fmap listh . P.readFile . hlist

writeFile :: FilePath -> Chars -> IO ()
writeFile n s =
    P.writeFile (hlist n) (hlist s)

getLine :: IO Chars
getLine =
    P.fmap listh P.getLine

getArgs :: IO (List Chars)
getArgs =
    P.fmap (listh . P.fmap listh) E.getArgs

isPrefixOf :: Eq a => List a -> List a -> Bool
isPrefixOf Nil _ =
    True
isPrefixOf _ Nil =
    False
isPrefixOf (x :. xs) (y :. ys) =
    x == y && isPrefixOf xs ys

isEmpty :: List a -> Bool
isEmpty Nil =
    True
isEmpty (_ :. _) =
    False

span :: (a -> Bool) -> List a -> (List a, List a)
span p x =
    (takeWhile p x, dropWhile p x)

break :: (a -> Bool) -> List a -> (List a, List a)
break p =
    span (not . p)

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil =
    Nil
dropWhile p xs@(x :. xs') =
    if p x
        then dropWhile p xs'
        else xs

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil =
    Nil
takeWhile p (x :. xs) =
    if p x
        then x :. takeWhile p xs
        else Nil

zip :: List a -> List b -> List (a, b)
zip =
    zipWith (,)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (a :. as) (b :. bs) =
    f a b :. zipWith f as bs
zipWith _ _ _ =
    Nil

unfoldr :: (a -> Maybe (b, a)) -> a -> List b
unfoldr f a =
    case f a of
        Just (b, a') -> b :. unfoldr f a'
        Nothing -> Nil

lines :: Chars -> List Chars
lines =
    listh . P.fmap listh . P.lines . hlist

unlines :: List Chars -> Chars
unlines =
    listh . P.unlines . hlist . map hlist

words :: Chars -> List Chars
words =
    listh . P.fmap listh . P.words . hlist

unwords :: List Chars -> Chars
unwords =
    listh . P.unwords . hlist . map hlist

listOptional :: (a -> Maybe b) -> List a -> List b
listOptional _ Nil =
    Nil
listOptional f (h :. t) =
    let r = listOptional f t
     in case f h of
            Nothing -> r
            Just q -> q :. r

any :: (a -> Bool) -> List a -> Bool
any p =
    foldRight ((||) . p) False

all :: (a -> Bool) -> List a -> Bool
all p =
    foldRight ((&&) . p) True

or :: List Bool -> Bool
or =
    any id

and :: List Bool -> Bool
and =
    all id

elem :: Eq a => a -> List a -> Bool
elem x =
    any (== x)

notElem :: Eq a => a -> List a -> Bool
notElem x =
    all (/= x)

permutations :: List a -> List (List a)
permutations xs0 =
    let perms Nil _ =
            Nil
        perms (t :. ts) is =
            let interleave' _ Nil r =
                    (ts, r)
                interleave' f (y :. ys) r =
                    let (us, zs) = interleave' (f . (y :.)) ys r
                     in (y :. us, f (t :. y :. us) :. zs)
             in foldRight (\xs -> snd . interleave' id xs) (perms ts (t :. is)) (permutations is)
     in xs0 :. perms xs0 Nil

intersectBy :: (a -> b -> Bool) -> List a -> List b -> List a
intersectBy e xs ys =
    filter (\x -> any (e x) ys) xs

take :: (Num n, Ord n) => n -> List a -> List a
take n _
    | n <= 0 =
        Nil
take _ Nil =
    Nil
take n (x :. xs) =
    x :. take (n - 1) xs

drop :: (Num n, Ord n) => n -> List a -> List a
drop n xs
    | n <= 0 =
        xs
drop _ Nil =
    Nil
drop n (_ :. xs) =
    drop (n - 1) xs

repeat :: a -> List a
repeat x =
    x :. repeat x

replicate :: (Num n, Ord n) => n -> a -> List a
replicate n x =
    take n (repeat x)

reads :: P.Read a => Chars -> Maybe (a, Chars)
reads s =
    case P.reads (hlist s) of
        [] -> Nothing
        ((a, q) : _) -> Just (a, listh q)

read :: P.Read a => Chars -> Maybe a
read =
    mapOptional fst . reads

readHexs :: (Eq a, Num a) => Chars -> Maybe (a, Chars)
readHexs s =
    case N.readHex (hlist s) of
        [] -> Nothing
        ((a, q) : _) -> Just (a, listh q)

readHex :: (Eq a, Num a) => Chars -> Maybe a
readHex =
    mapOptional fst . readHexs

readFloats :: (RealFrac a) => Chars -> Maybe (a, Chars)
readFloats s =
    case N.readSigned N.readFloat (hlist s) of
        [] -> Nothing
        ((a, q) : _) -> Just (a, listh q)

readFloat :: (RealFrac a) => Chars -> Maybe a
readFloat =
    mapOptional fst . readFloats

instance (a ~ Char) => IsString (List a) where
    -- Per https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.String.html#line-43
    fromString =
        listh

type Chars =
    List Char

type FilePath =
    List Char

strconcat :: [Chars] -> P.String
strconcat =
    P.concatMap hlist

stringconcat :: [P.String] -> P.String
stringconcat =
    P.concat

show' :: Show a => a -> List Char
show' =
    listh . show

instance P.Functor List where
    fmap f =
        listh . P.fmap f . hlist

instance A.Applicative List where
    (<*>) =
        M.ap
    pure =
        (:. Nil)

instance P.Monad List where
    (>>=) =
        flip flatMap
