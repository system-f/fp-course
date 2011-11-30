{-# LANGUAGE TypeOperators #-}

module L05.Testing where

import Prelude hiding (sum, length, map, all, filter, maximum, reverse)
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import L02.List


-- How to produce arbitrary instances of List
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap (foldr (:|) Nil) arbitrary

instance Show (a -> b) where
  show _ = "<function>"

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "List"
      [
        testProperty "map (identity)"           prop_map
      , testProperty "append_reverse"           prop_append_reverse
      , testProperty "append"                   prop_append
      , testProperty "foldRight"                prop_foldRight
      , testProperty "sum"                      prop_sum
      , testProperty "length"                   prop_length
      , testProperty "filter"                   prop_filter
      , testProperty "map (composition)"        prop_map_composition
      , testProperty "flatten"                  prop_flatten
      , testProperty "flatMap (associativity)"  prop_flatMap_associative
      , testProperty "maximum"                  prop_maximum
      , testProperty "maximum (contains)"       prop_maximum_contains
      , testProperty "reverse"                  prop_reverse
      ]
  ]

-- Mapping the identity function on any value (x) produces that value x.
prop_map ::
  List Int
  -> Bool
prop_map x =
  map id x == x

-- Appending x to y then reversing produces the same result as
-- reversing y and appending to the result of reversing x.
prop_append_reverse ::
  List Int
  -> List Int
  -> Bool
prop_append_reverse x y =
  reverse (append x y) == append (reverse y) (reverse x)

-- Exercise 1
-- Appending (x to y) to z produces the same result as
-- appending x to (y to z).
-- The law of associativity.
prop_append ::
  List Int
  -> List Int
  -> List Int
  -> Bool
prop_append x y z =
  (x `append` y) `append` z ==
  x `append` (y `append` z)

-- Exercise 2
-- Folding (right) with cons and nil on a list (x) produces that same list x.
prop_foldRight ::
  List Int
  -> Bool
prop_foldRight x =
  foldRight (:|) Nil x == x

-- Exercise 3
-- Folding on a list (x) with subtraction on the sum of x produces 0.
prop_sum ::
  List Int
  -> Bool
prop_sum x =
  foldLeft (-) (sum x) x == 0

-- Exercise 4
-- Replace each element in a list (x) with 1, then sum that list and you will have the length.
prop_length ::
  List Int
  -> Bool
prop_length x =
  sum (map (const 1) x) == length x

-- Exercise 5
-- Filtering a list (x) with a predicate (f) produces a list of elements where
-- all satisfy predicate f. /See all function below./
prop_filter ::
  (Int -> Bool)
  -> List Int
  -> Bool
prop_filter f x =
  all f (filter f x)

-- Exercise 6
-- Mapping a function (g) on a list (x), then mapping another function (f) on that result,
-- produces the same outcome as mapping the composition of f and g on x.
prop_map_composition ::
  (Int -> String)
  -> (Char -> Int)
  -> List Char
  -> Bool
prop_map_composition f g x =
  map f (map g x) == map (f . g) x

-- Exercise 7
-- Mapping length on a list (x) then taking the same produces the same result as
-- flattening the list and taking the length.
prop_flatten ::
  List (List Int)
  -> Bool
prop_flatten x =
  sum (map length x) == length (flatten x)

-- Exercise 8
-- Using (>>>=>) expressed in terms of flatMap, show that
-- (>>>=>) is an associative binary operation.
prop_flatMap_associative ::
  (Int -> List String)
  -> (String -> List Char)
  -> (Char -> List Integer)
  -> Int
  -> Bool
prop_flatMap_associative x y z a =
  let p >>>=> q = \k -> q `flatMap` p k
      d = (x >>>=> y) >>>=> z
      e = x >>>=> (y >>>=> z)
  in d a == e a

-- Exercise 9
-- All elements in maximum of a list (x) are less then or equal to x.
prop_maximum ::
  List Int
  -> Bool
prop_maximum x =
  all (\a -> maximum x >= a) x

-- Exercise 10
-- The maximum of a list (x) contains x.
-- Ensure safety with regard to the empty list.
prop_maximum_contains ::
  List Int
  -> Property
prop_maximum_contains x =
  (not . isEmpty $ x) ==>
  x `contains` maximum x

-- Exercise 11
-- Reversing a list (x) then reversing again results in x.
prop_reverse ::
  List Int
  -> Bool
prop_reverse x =
  (reverse . reverse) x == x
