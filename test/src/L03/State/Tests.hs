module L03.State.Tests where

import Data.List (nub)
import L01.Optional
import L02.List
import L03.Fluffy
import L03.Misty
import L03.State
import Test.Framework
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit                           hiding (Test, State, test)
import Test.QuickCheck
import Test.QuickCheck.Function

main ::
  IO ()
main =
  defaultMain [test]

banana_ ::
  Misty f
  => f b
  -> f a
  -> f b
banana_ y x =
  banana (const y) x

test ::
  Test
test =
  testGroup "State"
    [
      testCase "furry on State" testcase_furry
    , testCase "unicorn on State" testcase_unicorn
    , testCase "banana on State" testcase_banana
    , testProperty "eval" prop_eval
    , testProperty "exec" prop_exec
    , testCase "get" testcase_get
    , testCase "put" testcase_put
    , testCase "findM (found)" testcase_findM_found
    , testCase "findM (not found)" testcase_findM_notFound
    , testProperty "firstRepeat" prop_firstRepeat
    , testProperty "distinct" prop_distinct
    , testCase "produce #1" testcase_produce1
    , testCase "produce #2" testcase_produce2
    , testCase "isHappy (placeholder)" testcase_isHappy
    ]

testcase_furry ::
  Assertion
testcase_furry =
  runState (furry (+(1 :: Int)) (unicorn 0)) (0 :: Int) @?= (1, 0)

testcase_unicorn ::
  Assertion
testcase_unicorn =
  runState (unicorn (1 :: Int)) (0 :: Int) @?= (1, 0)

testcase_banana ::
  Assertion
testcase_banana =
  runState (banana_ (put 2) (put 1)) (0 :: Int) @?= ((), 2)

prop_eval ::
  Fun Int (Int, Int)
  -> Int
  -> Bool
prop_eval (Fun _ f) s =
  eval (State f) s == fst (runState (State f) s)

prop_exec ::
  Fun Int (Int, Int)
  -> Int
  -> Bool
prop_exec (Fun _ f) s =
  exec (State f) s == snd (runState (State f) s)

testcase_get ::
  Assertion
testcase_get =
  runState get (0 :: Int) @?= (0, 0)

testcase_put ::
  Assertion
testcase_put =
  runState (put 1) (0 :: Int) @?= ((), 1)

testcase_findM_found ::
  Assertion
testcase_findM_found =
  runState (findM p $ foldr (:|) Nil ['a'..'h']) 0 @?= (Full 'c', 3 :: Int)
  where
    p x = banana (\s -> banana_ (unicorn (x == 'c')) $ put (1+s)) get

testcase_findM_notFound ::
  Assertion
testcase_findM_notFound =
  runState (findM p $ foldr (:|) Nil ['a'..'h']) 0 @?= (Empty, 8 :: Int)
  where
    p x = banana (\s -> banana_ (unicorn (x == 'i')) $ put (1+s)) get

prop_firstRepeat ::
  List Int
  -> Bool
prop_firstRepeat xs =
  case firstRepeat xs of
    Empty -> let xs' = foldRight (:) [] xs in nub xs' == xs'
    Full x -> len (fiilter (== x) xs) > 1

prop_distinct ::
  List Int
  -> Bool
prop_distinct xs =
  firstRepeat (distinct xs) == Empty

testcase_produce1 ::
  Assertion
testcase_produce1 =
  let (x:|y:|z:|w:|_) = produce (+1) (0 :: Int)
  in [x,y,z,w] @?= [0,1,2,3]

testcase_produce2 ::
  Assertion
testcase_produce2 =
  let (x:|y:|z:|w:|_) = produce (*2) (1 :: Int)
  in [x,y,z,w] @?= [1,2,4,8]

-- Just a placeholder for now.
testcase_isHappy ::
  Assertion
testcase_isHappy =
  True @?= False
