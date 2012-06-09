{-# LANGUAGE TypeOperators #-}

module L05.Testing where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import L02.List

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "List"
      [
        testProperty "maap (identity)"          prop_maap
      , testProperty "append_rev"               prop_append_rev
      , testProperty "append"                   prop_append
      , testProperty "foldRight"                prop_foldRight
      , testProperty "suum"                     prop_suum
      , testProperty "len"                      prop_len
      , testProperty "fiilter"                  prop_fiilter
      , testProperty "maap (composition)"       prop_maap_composition
      , testProperty "flatten"                  prop_flatten
      , testProperty "flatMap (associativity)"  prop_flatMap_associative
      , testProperty "seqf"                     prop_seqf
      , testProperty "rev"                      prop_rev
      ]
  ]

-- Mapping the identity function on any value (x) produces that value x.
prop_maap ::
  List Int
  -> Bool
prop_maap x =
  maap id x == x

-- Appending x to y then reversing produces the same result as
-- reversing y and appending to the result of reversing x.
prop_append_rev ::
  List Int
  -> List Int
  -> Bool
prop_append_rev x y =
  rev (append x y) == append (rev y) (rev x)

-- Exercise 1
-- Appending (x to y) to z produces the same result as
-- appending x to (y to z).
-- The law of associativity.
prop_append ::
  List Int
  -> List Int
  -> List Int
  -> Bool
prop_append =
  error "todo"

-- Exercise 2
-- Folding (right) with cons and nil on a list (x) produces that same list x.
prop_foldRight ::
  List Int
  -> Bool
prop_foldRight =
  error "todo"

-- Exercise 3
-- Folding on a list (x) with subtraction on the sum of x produces 0.
prop_suum ::
  List Int
  -> Bool
prop_suum =
  error "todo"

-- Exercise 4
-- Replace each element in a list (x) with 1, then sum that list and you will have the length.
prop_len ::
  List Int
  -> Bool
prop_len =
  error "todo"

-- Exercise 5
-- Filtering a list (x) with a predicate (f) produces a list of elements where
-- all satisfy predicate f. /See all function below./
prop_fiilter ::
  Fun Int Bool
  -> List Int
  -> Bool
prop_fiilter =
  error "todo"

-- Exercise 6
-- Mapping a function (g) on a list (x), then mapping another function (f) on that result,
-- produces the same outcome as mapping the composition of f and g on x.
prop_maap_composition ::
  Fun Int String
  -> Fun Char Int
  -> List Char
  -> Bool
prop_maap_composition =
  error "todo"

-- Exercise 7
-- Mapping length on a list (x) then taking the same produces the same result as
-- flattening the list and taking the length.
prop_flatten ::
  List (List Int)
  -> Bool
prop_flatten =
  error "todo"

-- Exercise 8
-- Using (>>>=>) expressed in terms of flatMap, show that
-- (>>>=>) is an associative binary operation.
prop_flatMap_associative ::
  Fun Int (List String)
  -> Fun String (List Char)
  -> Fun Char (List Integer)
  -> Int
  -> Bool
prop_flatMap_associative =
  error "todo"

-- Exercise 9
-- Calling seqf on a list with length x, produces a list with length x.
prop_seqf ::
  List (Fun Int String)
  -> Int
  -> Bool
prop_seqf =
  error "todo"

-- Exercise 10
-- Reversing a list (x) then reversing again results in x.
prop_rev ::
  List Int
  -> Bool
prop_rev =
  error "todo"
