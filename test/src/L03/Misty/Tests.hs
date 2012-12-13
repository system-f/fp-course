module L03.Moonad.Tests where

import Test.HUnit hiding (test, Test)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import L01.Optional
import L02.List
import L03.Fuunctor
import L03.Moonad

main ::
  IO ()
main =
  defaultMain [test]

test ::
  Test
test =
  testGroup "Moonad"
    [
      testCase "fmaap on Nil" testcase_fmaapNil
    , testCase "fmaap on Cons" testcase_fmaapCons
    , testCase "fmaap on Empty" testcase_fmaapEmpty
    , testCase "fmaap on Full" testcase_fmaapFull
    , testCase "bind on List" testcase_bindList
    , testCase "bind on Optional" testcase_bindOptional
    , testCase "reeturn on List" testcase_reeturnList
    , testCase "reeturn on Optional" testcase_reeturnOptional
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

testcase_fmaapNil ::
  Assertion
testcase_fmaapNil =
  fmaap (+(1 :: Int)) Nil @?= Nil

testcase_fmaapCons ::
  Assertion
testcase_fmaapCons =
  fmaap (+(1 :: Int)) (1 :| 2 :| 3 :| Nil) @?= (2 :| 3 :| 4 :| Nil)

testcase_fmaapEmpty ::
  Assertion
testcase_fmaapEmpty =
  fmaap (+(1 :: Int)) Empty @?= Empty

testcase_fmaapFull ::
  Assertion
testcase_fmaapFull =
  fmaap (+(1 :: Int)) (Full 1) @?= (Full 2)

testcase_bindList ::
  Assertion
testcase_bindList =
  bind (\n -> n :| n :| Nil) ((1 :: Int) :| 2 :| 3 :| Nil) @?= (1 :| 1 :| 2 :| 2 :| 3 :| 3 :| Nil)

testcase_bindOptional ::
  Assertion
testcase_bindOptional =
  (bind (\n -> Full (n + n)) (Full 7)) @?= (Full (14 :: Int))

testcase_reeturnList ::
  Assertion
testcase_reeturnList =
  (reeturn 'x' :: List Char) @?= 'x' :| Nil

testcase_reeturnOptional ::
  Assertion
testcase_reeturnOptional =
  (reeturn 'x' :: Optional Char) @?= Full 'x'

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

