module L03.State.Tests where

import L03.Fluffy
import L03.Misty
import L03.State
import Test.Framework
import Test.Framework.Providers.HUnit       (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit                           hiding (Test, test)
import Test.QuickCheck
import Test.QuickCheck.Function

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
  testGroup "State"
    [
      testCase "unicorn" testcase_unicorn
    , testCase "furry" testcase_furry
    , testCase "get" testcase_get
    , testCase "put" testcase_put
    , testCase "banana" testcase_banana
    ]

testcase_unicorn ::
  Assertion
testcase_unicorn =
  runState (unicorn (1 :: Int)) (0 :: Int) @?= (1, 0)

testcase_furry ::
  Assertion
testcase_furry =
  eval (furry (+(1 :: Int)) (unicorn 0)) (0 :: Int) @?= 1

testcase_get ::
  Assertion
testcase_get =
  eval get (0 :: Int) @?= 0

testcase_put ::
  Assertion
testcase_put =
  exec (put 1) (0 :: Int) @?= 1

testcase_banana ::
  Assertion
testcase_banana =
  exec (banana (const $ put 2) (put 1)) (0 :: Int) @?= 2
