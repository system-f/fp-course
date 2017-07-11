{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.StateTTest where

import qualified Prelude            as P ((++))

import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           Course.Applicative (pure, (<*>))
import           Course.Core
import           Course.ExactlyOne  (ExactlyOne (..))
import           Course.Functor     ((<$>))
import           Course.List        (List (..))
import           Course.Monad       ((=<<), (>>=))
import           Course.Optional    (Optional (..))
import           Course.State       (put, runState)
import           Course.StateT      (StateT (..), putT, state')

test_StateT :: TestTree
test_StateT =
  testGroup "StateT" [
    functorTest
  , applicativeTest
  , monadTest
  , state'Test
  ]

functorTest :: TestTree
functorTest =
  testCase "<$>" $
    runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0 @?= ((3,0) :. Nil)

applicativeTest :: TestTree
applicativeTest =
  testGroup "Applicative" [
    testCase "List (pure)" $ runStateT ((pure 2) :: StateT Int List Int) 0 @?= ((2,0) :. Nil)
  , testCase "List (<*>)" $ runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0 @?= ((4,0) :. Nil)
  , testCase "Optional" $
      let st = StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))
       in runStateT st [0] @?= Full (4,[0,1,2])
  , testCase "List" $
      let st =     StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil)
               <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))
       in runStateT st [0] @?= ((4,[0,1,2]) :. (5,[0,1,2]) :. Nil)
  ]

monadTest :: TestTree
monadTest =
  testGroup "Monad" [
    testCase "bind const" $
      runStateT (const (putT 2) =<< putT 1) 0 @?= (((), 2) :. Nil)
  , testCase "modify" $
      let modify f = StateT (\s -> pure ((), f s))
       in runStateT (modify (+1) >>= \() -> modify (*2)) 7 @?= (((), 16) :. Nil)
  ]

state'Test :: TestTree
state'Test =
  testCase "state'" $
    runStateT (state' $ runState $ put 1) 0 @?= ExactlyOne ((), 1)
