{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.StateTest where

import           Data.List                (nub)
import qualified Prelude                  as P ((++))

import           Test.QuickCheck.Function (Fun (..))
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.HUnit         (testCase, (@?=))
import           Test.Tasty.QuickCheck    (forAllShrink, testProperty)

import           Course.Applicative       (pure, (<*>))
import           Course.Core
import           Course.Functor           ((<$>))
import           Course.List              (List (..), filter, flatMap, hlist,
                                           length, listh, span, (++))
import           Course.ListTest          (genIntegerList, shrinkList)
import           Course.Monad
import           Course.Optional          (Optional (..))
import           Course.State             (State (..), distinct, eval, exec,
                                           findM, firstRepeat, get, isHappy,
                                           put, put, runState)

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
execTest =
  testProperty "exec" $
    \(Fun _ f :: Fun Integer (Integer, Integer)) s -> exec (State f) s == snd (runState (State f) s)

evalTest :: TestTree
evalTest =
  testProperty "eval" $
    \(Fun _ f :: Fun Integer (Integer, Integer)) s -> eval (State f) s == fst (runState (State f) s)

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
      runState ((const $ put 2) =<< put 1) 0 @?= ((),2)
  , testCase "(>>=)" $
      let modify f = State (\s -> ((), f s))
       in runState (modify (+1) >>= \() -> modify (*2)) 7  @?= ((),16)
  ]

findMTest :: TestTree
findMTest =
  testGroup "findM" [
    testCase "find 'c' in 'a'..'h'" $
      let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get
       in runState (findM p $ listh ['a'..'h']) 0 @?= (Full 'c',3)
  , testCase "find 'i' in 'a'..'h'" $
      let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get
       in runState (findM p $ listh ['a'..'h']) 0 @?= (Empty,8)
  ]

firstRepeatTest :: TestTree
firstRepeatTest =
  testGroup "firstRepeat" [
    testProperty "finds repeats" $ forAllShrink genIntegerList shrinkList (\xs ->
      case firstRepeat xs of
        Empty ->
          let xs' = hlist xs
           in nub xs' == xs'
        Full x -> length (filter (== x) xs) > 1
    )
  , testProperty "" $ forAllShrink genIntegerList shrinkList (\xs ->
      case firstRepeat xs of
        Empty -> True
        Full x ->
          let (l, (rx :. rs)) = span (/= x) xs
           in let (l2, _) = span (/= x) rs
               in let l3 = hlist (l ++ (rx :. Nil) ++ l2)
                   in nub l3 == l3
    )
  ]

distinctTest :: TestTree
distinctTest =
  testGroup "distinct" [
    testProperty "No repeats after distinct" $
      forAllShrink genIntegerList shrinkList (\xs -> firstRepeat (distinct xs) == Empty)
  , testProperty "" $
      forAllShrink genIntegerList shrinkList (\xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs))
  ]

isHappyTest :: TestTree
isHappyTest =
  testGroup "isHappy" [
    testCase "4" $ isHappy 4 @?= False
  , testCase "7" $ isHappy 7 @?= True
  , testCase "42" $ isHappy 42 @?=  False
  , testCase "44" $ isHappy 44 @?=  True
  ]
