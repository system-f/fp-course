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
    , testCase "flaatten on List" testcase_flaattenList
    , testCase "flaatten on Optional (Full then Empty)" testcase_flaattenOptionalF
    , testCase "flaatten on Optional (Full then Full)" testcase_flaattenOptionalFF
    , testCase "seequence on List" testcase_seequenceList
    , testCase "seequence on Optional (Full then Empty)" testcase_seequenceOptionalF
    , testCase "seequence on Optional (Full then Full)" testcase_seequenceOptionalFF
    , testCase "traaverse on List" testcase_traaverseList
    , testCase "traaverse on Optional (Full then Empty)" testcase_traaverseOptionalF
    , testCase "traaverse on Optional (Full then Full)" testcase_traaverseOptionalFF
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

testcase_flaattenList ::
  Assertion
testcase_flaattenList =
  flaatten ((1 :| 2 :| 3 :| Nil) :| ((1 :: Int) :| 2 :| Nil) :| Nil) @?= 1 :| 2 :| 3 :| 1 :| 2 :| Nil

testcase_flaattenOptionalF ::
  Assertion
testcase_flaattenOptionalF =
  flaatten (Full (Empty::Optional Int)) @?= Empty

testcase_flaattenOptionalFF ::
  Assertion
testcase_flaattenOptionalFF =
  flaatten (Full (Full 7)) @?= Full (7::Int)

testcase_seequenceList ::
  Assertion
testcase_seequenceList =
  seequence [(1::Int) :| 2 :| 3 :| Nil, 1 :| 2 :| Nil] @?= [1, 1] :| [1, 2] :| [2, 1] :| [2, 2] :| [3, 1] :| [3, 2] :| Nil

testcase_seequenceOptionalF ::
  Assertion
testcase_seequenceOptionalF =
  seequence [Full (7::Int), Empty] @?= Empty

testcase_seequenceOptionalFF ::
  Assertion
testcase_seequenceOptionalFF =
  seequence [Full 7, Full 8] @?= Full [7, 8::Int]

testcase_traaverseList ::
  Assertion
testcase_traaverseList =
  traaverse id [1 :| 2 :| 3 :| Nil, 1 :| 2 :| Nil] @?= [1, 1] :| [1, 2] :| [2, 1] :| [2, 2] :| [3, 1] :| [3, 2::Int] :| Nil

testcase_traaverseOptionalF ::
  Assertion
testcase_traaverseOptionalF =
  traaverse id [Full (7::Int), Empty] @?= Empty

testcase_traaverseOptionalFF ::
  Assertion
testcase_traaverseOptionalFF =
  traaverse id [Full 7, Full 8] @?= Full [7, 8::Int]

