{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateTTest (
  -- * Tests
    test_StateT
  , functorTest
  , applicativeTest
  , monadTest
  , state'Test
  , runState'Test
  , execTTest
  , exec'Test
  , evalTTest
  , eval'Test
  , getTTest
  , putTTest
  , distinct'Test
  , distinctFTest
  , optionalTFunctorTest
  , optionalTApplicativeTest
  , optionalTMonadTest
  , loggerFunctorTest
  , loggerApplicativeTest
  , loggerMonadTest
  , log1Test
  , distinctGTest

  -- * Course test runner
  , courseTest
  ) where

import qualified Prelude            as P (String, (++))

import           Test.Course.Mini   (courseTest)
import           Test.Mini          (MiniTestTree, fn, testCase, testGroup,
                                     testProperty, (@?=))

import           Course.Applicative (pure, (<*>))
import           Course.Core
import           Course.ExactlyOne  (ExactlyOne (ExactlyOne))
import           Course.Functor     ((<$>))
import           Course.Gens        (genInteger, genList)
import           Course.List        (List ((:.), Nil), flatMap, listh)
import           Course.Monad       ((=<<), (>>=))
import           Course.Optional    (Optional (Empty, Full))
import           Course.State       (put, runState)
import           Course.StateT      (Logger (Logger), OptionalT (OptionalT),
                                     StateT (StateT, runStateT), distinct',
                                     distinctF, distinctG, eval', evalT, exec',
                                     execT, getT, log1, putT, runOptionalT,
                                     runState', state')

test_StateT :: MiniTestTree
test_StateT =
  testGroup "StateT" [
    functorTest
  , applicativeTest
  , monadTest
  , state'Test
  , runState'Test
  , execTTest
  , exec'Test
  , evalTTest
  , eval'Test
  , getTTest
  , putTTest
  , distinct'Test
  , distinctFTest
  , optionalTFunctorTest
  , optionalTApplicativeTest
  , optionalTMonadTest
  , loggerFunctorTest
  , loggerApplicativeTest
  , loggerMonadTest
  , log1Test
  , distinctGTest
  ]

functorTest :: MiniTestTree
functorTest =
  testCase "<$>" $
    let st = StateT (\s -> (2, s) :. Nil)
     in runStateT ((+1) <$> st) 0 @?= ((3,0) :. Nil)

applicativeTest :: MiniTestTree
applicativeTest =
  testGroup "Applicative" [
    testCase "List (pure)" $ runStateT (pure 2 :: StateT Int List Int) 0 @?= ((2,0) :. Nil)
  , testCase "List (<*>)" $ runStateT (pure (+2) <*> (pure 2 :: StateT Int List Int)) 0 @?= ((4,0) :. Nil)
  , testCase "Optional" $
      let st = StateT (\s -> Full ((+2), s P.++ [1])) <*> StateT (\s -> Full (2, s P.++ [2]))
       in runStateT st [0] @?= Full (4,[0,1,2])
  , testCase "List" $
      let st =     StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil)
               <*> StateT (\s -> (2, s P.++ [2]) :. Nil)
       in runStateT st [0] @?= ((4,[0,1,2]) :. (5,[0,1,2]) :. Nil)
  ]

monadTest :: MiniTestTree
monadTest =
  testGroup "Monad" [
    testCase "bind const" $
      let s n = StateT $ const (((), n) :. Nil)
       in runStateT (const (s 2) =<< s 1) 0 @?= (((), 2) :. Nil)
  , testCase "modify" $
      let modify f = StateT (\s -> pure ((), f s))
       in runStateT (modify (+1) >>= \() -> modify (*2)) 7 @?= (((), 16) :. Nil)
  ]

state'Test :: MiniTestTree
state'Test =
  testCase "state'" $
    runStateT (state' . runState $ put 1) 0 @?= ExactlyOne ((), 1)

runState'Test :: MiniTestTree
runState'Test =
  testCase "runState'" $
    runState' (state' . runState $ put 1) 0 @?= ((),1)

execTTest :: MiniTestTree
execTTest =
  testCase "execTTest" $
    execT (StateT $ \s -> Full ((), s + 1)) 2 @?= Full 3

exec'Test :: MiniTestTree
exec'Test =
  testCase "exec'Test" $
    exec' (state' $ \s -> ((), s + 1)) 2 @?= 3

evalTTest :: MiniTestTree
evalTTest =
  testCase "evalTTest" $
    evalT (StateT $ \s -> Full (even s, s + 1)) 2 @?= Full True

eval'Test :: MiniTestTree
eval'Test =
  testCase "eval'Test" $
    eval' (state' $ \s -> (even s, s + 1)) 5 @?= False

getTTest :: MiniTestTree
getTTest =
  testCase "getTTest" $
    runStateT (getT :: StateT Int List Int) 3 @?= ((3,3) :. Nil)

putTTest :: MiniTestTree
putTTest =
  testCase "putTTest" $
    runStateT (putT 2 :: StateT Int List ()) 0 @?= (((),2) :. Nil)

distinct'Test :: MiniTestTree
distinct'Test =
  testGroup "distinct'" [
    testCase "removes duplicate 'c's" $
      distinct' (listh "abcdcefcghi") @?= listh ['a'..'i']
  , testProperty "distinct'" . fn (genList genInteger) $ \xs ->
      distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
  ]

distinctFTest :: MiniTestTree
distinctFTest =
  testGroup "distinctF" [
    testCase "Full case" $ distinctF (listh [1,2,3,2,1]) @?= Full (listh [1,2,3])
  , testCase "Empty case" $ distinctF (listh [1,2,3,2,1,101]) @?= Empty
  ]

optionalTFunctorTest :: MiniTestTree
optionalTFunctorTest =
  testCase "(<$>) for OptionalT" $
    runOptionalT ((+1) <$> OptionalT (Full 1 :. Empty :. Nil)) @?= (Full 2 :. Empty :. Nil)

optionalTApplicativeTest :: MiniTestTree
optionalTApplicativeTest =
  testGroup "(<*>) for OptionalT" [
    testCase "one" $
      let ot = (OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil))
       in runOptionalT ot @?= (Nil :: List (Optional Int))
  , testCase "two" $
      let ot = OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
       in runOptionalT ot @?= (Nil :: List (Optional Int))
  , testCase "three" $
      let ot = OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
       in runOptionalT ot @?= (Empty :. Nil :: List (Optional Int))
  , testCase "four" $
      let ot = OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
       in runOptionalT ot @?= (Empty :. Empty :. Nil :: List (Optional Int))
  , testCase "five" $
      let ot = OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
       in runOptionalT ot @?= (Empty :. Nil :: List (Optional Int))
  , testCase "six" $
      let ot = OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
       in runOptionalT ot @?= (Full 2 :. Full 3 :. Empty :. Nil)
  , testCase "seven" $
      let ot = OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
       in runOptionalT ot @?= (Full 2 :. Empty :. Full 3 :. Empty :. Nil)
  ]

optionalTMonadTest :: MiniTestTree
optionalTMonadTest =
  testCase "(=<<) for OptionalT" $
    let ot = (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
     in runOptionalT ot @?= (Full 2:.Full 3:.Empty:.Nil)

loggerFunctorTest :: MiniTestTree
loggerFunctorTest =
  testCase "(<$>) for Logger" $
    (+3) <$> Logger (1 :. 2 :. Nil) 3 @?= Logger (1 :. 2 :. Nil) 6

loggerApplicativeTest :: MiniTestTree
loggerApplicativeTest =
  testGroup "Logger Applicative" [
    testCase "pure" $
      (pure "table" :: Logger Int P.String) @?= Logger Nil "table"
  , testCase "<*>" $
      Logger (1:.2:.Nil) (+7) <*> Logger (3:.4:.Nil) 3 @?= Logger (1:.2:.3:.4:.Nil) 10
  ]

loggerMonadTest :: MiniTestTree
loggerMonadTest =
  testCase "(=<<) for Logger" $
    ((\a -> Logger (4:.5:.Nil) (a+3)) =<< Logger (1:.2:.Nil) 3) @?= Logger (1:.2:.4:.5:.Nil) 6

log1Test :: MiniTestTree
log1Test =
  testCase "log1" $
    log1 1 2 @?= Logger (1:.Nil) 2

distinctGTest :: MiniTestTree
distinctGTest =
  testGroup "distinctG" [
    testCase "Full case" $
      let expected = Logger (listh <$> ("even number: 2":."even number: 2":."even number: 6":.Nil))
                            (Full (1:.2:.3:.6:.Nil))
       in distinctG (1:.2:.3:.2:.6:.Nil) @?= expected
  , testCase "Empty case" $
      let expected = Logger (listh <$> ("even number: 2":."even number: 2":."even number: 6":."aborting > 100: 106":.Nil)) Empty
       in distinctG (listh [1,2,3,2,6,106]) @?= expected
  ]
