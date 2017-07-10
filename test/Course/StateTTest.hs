{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.StateTTest where

import qualified Prelude            as P ((++))

import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           Course.Applicative (pure, (<*>))
import           Course.Core
import           Course.Functor     ((<$>))
import           Course.List        (List (..))
import           Course.Optional    (Optional (..))
import           Course.StateT      (StateT (..))

test_StateT :: TestTree
test_StateT =
  testGroup "StateT" [
    functorTest
  , applicativeTest
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
