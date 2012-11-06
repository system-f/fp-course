module L02.List.Tests where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import L02.List
import Test.QuickCheck
import Test.QuickCheck.Function

main :: 
  IO ()
main = 
  defaultMain [test]

test :: 
  Test
test =
  testGroup "List"
    [
      testProperty "headOr defaults with Nil" prop_headOr_Nil
    , testProperty "headOr uses head with non-empty" prop_headOr_Cons
    , testProperty "sum reduces to zero with subtraction" prop_suum
    , testProperty "length reduces to zero with subtraction" prop_len
    , testProperty "map obeys law of identity" prop_map_identity
    , testProperty "map obeys law of composition" prop_map_composition
    , testProperty "filter leaves only valid values" prop_fiilter
    , testProperty "append produces a list with the sum of the lengths" prop_append
    , testProperty "flatten sums the lengths" prop_flatten
    , testProperty "flatMap obeys law of left identity" prop_flatMap_left_identity
    , testProperty "flatMap obeys law of right identity" prop_flatMap_right_identity
    , testProperty "flatMap obeys law of associativity" prop_flatMap_associativity
    , testProperty "flatMap with id flattens" prop_flatMap_id_flattens
    , testProperty "flatMap obeys functor relationship" prop_flatMap_functor
    , testProperty "rev with single value" prop_rev_single_value
    , testProperty "appending reverse is equal to reversing appended" prop_rev_append
    ]

prop_headOr_Nil ::
  Int
  -> Bool
prop_headOr_Nil h =
  headOr Nil h == h

prop_headOr_Cons ::
  Int
  -> List Int
  -> Int
  -> Bool
prop_headOr_Cons h t x =
  headOr (h :| t) x == h

prop_suum ::
  List Int
  -> Bool
prop_suum x =
  foldLeft (-) (suum x) x == 0

prop_len ::
  List Int
  -> Bool
prop_len x =
  foldLeft (const . pred) (len x) x == 0

prop_map_identity ::
  List Int
  -> Bool
prop_map_identity x =
  maap id x == x

prop_map_composition ::
  Fun Int Int
  -> Fun Int Int
  -> List Int
  -> Bool
prop_map_composition (Fun _ f) (Fun _ g) x =
  maap f (maap g x) == maap (f . g) x

prop_fiilter ::
  Fun Int Bool
  -> List Int
  -> Bool
prop_fiilter (Fun _ p) x =
  foldRight (\a b -> p a && b) True (fiilter p x) &&
  foldRight (\a b -> not (p a) && b) True (fiilter (not . p) x)

prop_append ::
  List Int
  -> List Int
  -> Bool
prop_append x y =
  len x + len y == len (append x y)

prop_flatten ::
  List (List Int)
  -> Bool
prop_flatten x =
  len (flatten x) == suum (maap len x)

prop_flatMap_right_identity ::
  List Int
  -> Bool
prop_flatMap_right_identity x =
  flatMap (\n -> n :| Nil) x == x

prop_flatMap_left_identity ::
  Fun Int (List Int)
  -> Int
  -> Bool
prop_flatMap_left_identity (Fun _ f) n =
  flatMap f (n :| Nil) == f n

prop_flatMap_associativity ::
  Fun Int (List Int)
  -> Fun Int (List Int)
  -> List Int
  -> Bool
prop_flatMap_associativity (Fun _ f) (Fun _ g) x =
  flatMap g (flatMap f x) == flatMap (flatMap g . f) x

prop_flatMap_id_flattens ::
  List (List Int)
  -> Bool
prop_flatMap_id_flattens x =
  flatMap id x == flatten x

prop_flatMap_functor ::
  Fun Int Int
  -> List Int
  -> Bool
prop_flatMap_functor (Fun _ f) x =
  maap f x == flatMap (\w -> f w :| Nil) x

prop_rev_single_value ::
  Int
  -> Bool
prop_rev_single_value n =
  rev (n :| Nil) == (n :| Nil)

prop_rev_append ::
  List Int
  -> List Int
  -> Bool
prop_rev_append x y =
  append (rev x) (rev y) == rev (append y x)

