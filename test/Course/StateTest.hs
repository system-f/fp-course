{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.StateTest where

import qualified Prelude                  as P ((++))

import           Test.QuickCheck.Function (Fun (..))
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.HUnit         (testCase, (@?=))
import           Test.Tasty.QuickCheck    (testProperty)

import           Course.Core
import           Course.State             (State (..), exec, put, runState)

test_State :: TestTree
test_State =
  testGroup "State" [
    functorTest
  , applicativeTest
  , monadTest
  , execTest
  ]

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
      let state = (<*>) State (\s -> ((+3), s P.++ ["apple"]))
                        State (\s -> (7, s P.++ ["banana"]))
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

execTest :: TestTree
execTest =
  testProperty "exec" $
    \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
