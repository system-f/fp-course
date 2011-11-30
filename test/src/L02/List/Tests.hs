module L02.List.Tests where

import Prelude hiding (sum, length, map, filter, maximum, reverse)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import L02.List
import Test.QuickCheck
import Test.QuickCheck.Function

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    fmap (foldr (:|) Nil) arbitrary

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
    , testProperty "sum reduces to zero with subtraction" prop_sum
    , testProperty "length reduces to zero with subtraction" prop_length
    , testProperty "map obeys law of identity" prop_map_identity
    , testProperty "map obeys law of composition" prop_map_composition
    , testProperty "filter leaves only valid values" prop_filter
    , testProperty "append produces a list with the sum of the lengths" prop_append
    , testProperty "flatten sums the lengths" prop_flatten
    , testProperty "flatMap obeys law of left identity" prop_flatMap_left_identity
    , testProperty "flatMap obeys law of right identity" prop_flatMap_right_identity
    , testProperty "flatMap obeys law of associativity" prop_flatMap_associativity
    , testProperty "flatMap with id flattens" prop_flatMap_id_flattens
    , testProperty "flatMap obeys functor relationship" prop_flatMap_functor
    , testProperty "maximum is greater or equal to other elements" prop_maximum
    , testProperty "appending maximums is equal to maximum of appended" prop_maximum_append
    , testProperty "reverse with single value" prop_reverse_single_value
    , testProperty "appending reverse is equal to reversing appended" prop_reverse_append
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

prop_sum ::
  List Int
  -> Bool
prop_sum x =
  foldLeft (-) (sum x) x == 0

prop_length ::
  List Int
  -> Bool
prop_length x =
  foldLeft (const . pred) (length x) x == 0

prop_map_identity ::
  List Int
  -> Bool
prop_map_identity x =
  map id x == x

prop_map_composition ::
  Fun Int Int
  -> Fun Int Int
  -> List Int
  -> Bool
prop_map_composition (Fun _ f) (Fun _ g) x =
  map f (map g x) == map (f . g) x

prop_filter ::
  Fun Int Bool
  -> List Int
  -> Bool
prop_filter (Fun _ p) x =
  foldRight (\a b -> p a && b) True (filter p x) &&
  foldRight (\a b -> not (p a) && b) True (filter (not . p) x)

prop_append ::
  List Int
  -> List Int
  -> Bool
prop_append x y =
  length x + length y == length (append x y)

prop_flatten ::
  List (List Int)
  -> Bool
prop_flatten x =
  length (flatten x) == sum (map length x)

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
  map f x == flatMap (\w -> f w :| Nil) x

prop_maximum ::
  List Int
  -> Property 
prop_maximum x = 
  let m = maximum x
  in (x /= Nil) ==> foldRight (\a b -> m >= a && b) True x

prop_maximum_append ::
  List Int
  -> List Int
  -> Property
prop_maximum_append x y =
  (x /= Nil && y /= Nil) ==> maximum (maximum x :| maximum y :| Nil) == maximum (append x y)

prop_reverse_single_value ::
  Int
  -> Bool
prop_reverse_single_value n =
  reverse (n :| Nil) == (n :| Nil)

prop_reverse_append ::
  List Int
  -> List Int
  -> Bool
prop_reverse_append x y =
  append (reverse x) (reverse y) == reverse (append y x)

