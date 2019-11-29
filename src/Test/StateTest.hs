{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.StateTest (
  -- * Tests
    test_State
  , getTest
  , putTest
  , functorTest
  , applicativeTest
  , monadTest
  , findMTest
  , firstRepeatTest
  , distinctTest
  , isHappyTest
  , execTest
  , evalTest

  -- * Runner
  , test
  ) where

import           Data.List          (nub)
import qualified Prelude            as P ((++))

import           Test.Framework     (TestTree, testCase, testGroup,
                                     testProperty, test, (@?=))
import           Test.Framework.Property (Unshowable(..))

import           Course.Applicative (pure, (<*>))
import           Course.Core
import           Course.Functor     ((<$>))
import           Course.List        (List (..), filter, flatMap, hlist, length,
                                     listh, span, (++))
import           Course.Monad
import           Course.Optional    (Optional (Empty, Full))
import           Course.State       (State (State), distinct, eval, exec, findM,
                                     firstRepeat, get, isHappy, put, runState)

test_State :: TestTree
test_State =
  testGroup "State" [
    execTest
  , evalTest
  , getTest
  , putTest
  , functorTest
  , applicativeTest
  , monadTest
  , findMTest
  , firstRepeatTest
  , distinctTest
  , isHappyTest
  ]

execTest :: TestTree
execTest = testProperty "exec" $ \(Unshowable f) s ->
  exec (State f) s == snd (runState (State (f :: Int -> (Int, Int))) (s :: Int))

evalTest :: TestTree
evalTest = testProperty "eval" $ \(Unshowable f) s ->
  eval (State f) s == fst (runState (State (f :: Int -> (Int, Int))) (s :: Int))

getTest :: TestTree
getTest =
  testCase "get" $ runState get 0 @?= (0,0)

putTest :: TestTree
putTest =
  testCase "put" $ runState (put 1) 0 @?= ((),1)

functorTest :: TestTree
functorTest =
  testCase "(<$>)" $
    runState ((+1) <$> State (\s -> (9, s * 2))) 3 @?= (10,6)

applicativeTest :: TestTree
applicativeTest =
  testGroup "Applicative" [
    testCase "pure" $ runState (pure 2) 0 @?= (2,0)
  , testCase "<*>" $ runState (pure (+1) <*> pure 0) 0 @?= (1,0)
  , testCase "complicated <*>" $
      let state = State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))
       in runState state [] @?= (10,["apple","banana"])
  ]

monadTest :: TestTree
monadTest =
  testGroup "Monad" [
    testCase "(=<<)" $
      runState (const (put 2) =<< put 1) 0 @?= ((),2)
  , testCase "correctly produces new state and value" $
      runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2 @?= (10, 16)
  , testCase "(>>=)" $
      let modify f = State (\s -> ((), f s))
       in runState (modify (+1) >>= \() -> modify (*2)) 7  @?= ((),16)
  ]

findMTest :: TestTree
findMTest =
  testGroup "findM" [
    testCase "find 'c' in 'a'..'h'" $
      let p x = (\s -> const (pure (x == 'c')) =<< put (1+s)) =<< get
       in runState (findM p $ listh ['a'..'h']) 0 @?= (Full 'c',3)
  , testCase "find 'i' in 'a'..'h'" $
      let p x = (\s -> const (pure (x == 'i')) =<< put (1+s)) =<< get
       in runState (findM p $ listh ['a'..'h']) 0 @?= (Empty,8)
  ]

firstRepeatTest :: TestTree
firstRepeatTest =
  testGroup "firstRepeat" [
    testCase "'x' is the only repeat" $
      firstRepeat (listh "abxdexghi") @?= Full 'x'
  , testCase "'x' is the first repeat" $
      firstRepeat (listh "abxdexgg") @?= Full 'x'
  , testCase "no repeats" $
      firstRepeat (listh ['a'..'z']) @?= Empty
  , testProperty "finds repeats" $ \xs ->
      case firstRepeat (xs :: List Integer) of
        Empty ->
          let xs' = hlist xs
          in nub xs' == xs'
        Full x -> length (filter (== x) xs) > 1
  , testProperty "removing repeats matches nub" $ \xs ->
      case firstRepeat (xs :: List Integer) of
        Empty -> True
        Full x ->
          let
            (l, rx :. rs) = span (/= x) xs
            (l2, _) = span (/= x) rs
            l3 = hlist (l ++ rx :. l2)
          in
            nub l3 == l3
  ]

distinctTest :: TestTree
distinctTest =
  testGroup "distinct" [
    testCase "No repeats" $
      let cs = listh ['a'..'z'] in distinct cs @?= cs
  , testCase "Every element repeated" $
      let cs = listh ['a'..'z'] in distinct (flatMap (\x -> x :. x :. Nil) cs) @?= cs
  , testProperty "No repeats after distinct" $ \xs ->
      firstRepeat (distinct (xs :: List Integer)) == Empty
  , testProperty "Every element repeated" $ \xs ->
      distinct (xs :: List Integer) == distinct (flatMap (\x -> x :. x :. Nil) xs)
  ]

isHappyTest :: TestTree
isHappyTest =
  testGroup "isHappy" [
    testCase "4" $ isHappy 4 @?= False
  , testCase "7" $ isHappy 7 @?= True
  , testCase "42" $ isHappy 42 @?=  False
  , testCase "44" $ isHappy 44 @?=  True
  ]
