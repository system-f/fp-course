module L04.Misty.Tests where

import Test.HUnit hiding (test, Test)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import L01.Optional
import L02.List
import L04.Fluffy
import L04.Misty

main :: 
  IO ()
main = 
  defaultMain [test]

test :: 
  Test
test =
  testGroup "Parser"
    [
      testCase "furry on Nil" testcase_furryNil
    , testCase "furry on Cons" testcase_furryCons
    , testCase "furry on Empty" testcase_furryEmpty
    , testCase "furry on Full" testcase_furryFull
    , testCase "banana on List" testcase_bananaList
    , testCase "banana on Optional" testcase_bananaOptional
    , testCase "unicorn on List" testcase_unicornList
    , testCase "unicorn on Optional" testcase_unicornOptional
    , testCase "jellybean on List" testcase_jellybeanList
    , testCase "jellybean on Optional (Full then Empty)" testcase_jellybeanOptionalF
    , testCase "jellybean on Optional (Full then Full)" testcase_jellybeanOptionalFF
    , testCase "sausage on List" testcase_sausageList
    , testCase "sausage on Optional (Full then Empty)" testcase_sausageOptionalF
    , testCase "sausage on Optional (Full then Full)" testcase_sausageOptionalFF
    , testCase "moppy on List" testcase_moppyList
    , testCase "moppy on Optional (Full then Empty)" testcase_moppyOptionalF
    , testCase "moppy on Optional (Full then Full)" testcase_moppyOptionalFF
    ]

testcase_furryNil ::
  Assertion
testcase_furryNil =
  furry (+(1 :: Int)) Nil @?= Nil

testcase_furryCons ::
  Assertion
testcase_furryCons =
  furry (+(1 :: Int)) (1 :| 2 :| 3 :| Nil) @?= (2 :| 3 :| 4 :| Nil)

testcase_furryEmpty ::
  Assertion
testcase_furryEmpty =
  furry (+(1 :: Int)) Empty @?= Empty

testcase_furryFull ::
  Assertion
testcase_furryFull =
  furry (+(1 :: Int)) (Full 1) @?= (Full 2)

testcase_bananaList ::
  Assertion
testcase_bananaList =
  banana (\n -> n :| n :| Nil) ((1 :: Int) :| 2 :| 3 :| Nil) @?= (1 :| 1 :| 2 :| 2 :| 3 :| 3 :| Nil)

testcase_bananaOptional ::
  Assertion
testcase_bananaOptional =
  (banana (\n -> Full (n + n)) (Full 7)) @?= (Full (14 :: Int))

testcase_unicornList ::
  Assertion
testcase_unicornList =
  (unicorn 'x' :: List Char) @?= 'x' :| Nil

testcase_unicornOptional ::
  Assertion
testcase_unicornOptional =
  (unicorn 'x' :: Optional Char) @?= Full 'x'

testcase_jellybeanList ::
  Assertion
testcase_jellybeanList =
  jellybean ((1 :| 2 :| 3 :| Nil) :| ((1 :: Int) :| 2 :| Nil) :| Nil) @?= 1 :| 2 :| 3 :| 1 :| 2 :| Nil

testcase_jellybeanOptionalF ::
  Assertion
testcase_jellybeanOptionalF =
  jellybean (Full (Empty::Optional Int)) @?= Empty

testcase_jellybeanOptionalFF ::
  Assertion
testcase_jellybeanOptionalFF =
  jellybean (Full (Full 7)) @?= Full (7::Int)

testcase_sausageList ::
  Assertion
testcase_sausageList =
  sausage [(1::Int) :| 2 :| 3 :| Nil, 1 :| 2 :| Nil] @?= [1, 1] :| [1, 2] :| [2, 1] :| [2, 2] :| [3, 1] :| [3, 2] :| Nil

testcase_sausageOptionalF ::
  Assertion
testcase_sausageOptionalF =
  sausage [Full (7::Int), Empty] @?= Empty

testcase_sausageOptionalFF ::
  Assertion
testcase_sausageOptionalFF =
  sausage [Full 7, Full 8] @?= Full [7, 8::Int]

testcase_moppyList ::
  Assertion
testcase_moppyList =
  moppy id [1 :| 2 :| 3 :| Nil, 1 :| 2 :| Nil] @?= [1, 1] :| [1, 2] :| [2, 1] :| [2, 2] :| [3, 1] :| [3, 2::Int] :| Nil

testcase_moppyOptionalF ::
  Assertion
testcase_moppyOptionalF =
  moppy id [Full (7::Int), Empty] @?= Empty

testcase_moppyOptionalFF ::
  Assertion
testcase_moppyOptionalFF =
  moppy id [Full 7, Full 8] @?= Full [7, 8::Int]

