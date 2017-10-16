{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ParserTest where

import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           Course.Applicative ((*>))
import           Course.Core
import           Course.Parser      (ParseResult (Result, UnexpectedEof),
                                     ageParser, bindParser, character,
                                     constantParser, digit, firstNameParser,
                                     isErrorResult, list, list1, mapParser,
                                     parse, personParser, phoneBodyParser,
                                     phoneParser, satisfy, sequenceParser,
                                     smokerParser, surnameParser, thisMany,
                                     valueParser, (>>>), (|||))

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
    constantParserTest
  , valueParserTest
  , characterTest
  , mapParserTest
  , bindParserTest
  , ignoreFirstTest
  , alternationTest
  , listTest
  , list1Test
  , satisfyTest
  , sequenceParserTest
  , thisManyTest
  , ageParserTest
  , firstNameParserTest
  , surnameParserTest
  , smokerParserTest
  , phoneBodyParserTest
  , phoneParserTest
  , personParserTest
  ]

constantParserTest :: TestTree
constantParserTest =
  testGroup "constantParser" [
    testCase "Error result always an error" $
      isErrorResult (parse (constantParser UnexpectedEof) "abc") @?= True
  ]

valueParserTest :: TestTree
valueParserTest =
  testGroup "valueParser" [
    testCase "returns value, consumes no input" $
      parse (valueParser 3) "abc" @?= Result "abc" 3
  ]

characterTest :: TestTree
characterTest =
  testGroup "character" [
    testCase "parses a single character" $
      parse character "abc" @?= Result "bc" 'a'
  , testCase "fails on empty input" $
      isErrorResult (parse character "") @?= True
  ]

mapParserTest :: TestTree
mapParserTest =
  testGroup "mapParser" [
    testCase "character" $
      parse (mapParser succ character) "amz" @?= Result "mz" 'b'
  , testCase "valueParser" $
      parse (mapParser (+10) (valueParser 7)) "" @?= Result "" 17
  ]

bindParserTest :: TestTree
bindParserTest =
  testGroup "bindParser" [
    testCase "not 'x'" $
      parse (bindParser parseAfterX character) "abc" @?= Result "bc" 'v'
  , testCase "not 'x' - one input char" $
      parse (bindParser parseAfterX character) "a" @?= Result "" 'v'
  , testCase "'x' and next char parsed" $
      parse (bindParser parseAfterX character) "xabc" @?= Result "bc" 'a'
  , testCase "empty input is error" $
      isErrorResult (parse (bindParser parseAfterX character) "") @?= True
  , testCase "only x is error" $
      isErrorResult (parse (bindParser parseAfterX character) "x") @?= True
  ]
  where parseAfterX c = if c == 'x' then character else valueParser 'v'

ignoreFirstTest :: TestTree
ignoreFirstTest =
  testGroup ">>>" [
    testCase "character then value" $
      parse (character >>> valueParser 'v') "abc" @?= Result "bc" 'v'
  , testCase "no input is error" $
      isErrorResult (parse (character >>> valueParser 'v') "") @?= True
  ]

alternationTest :: TestTree
alternationTest =
  testGroup "|||" [
    testCase "char or 'v' - no char" $
      parse (character ||| valueParser 'v') "" @?= Result "" 'v'
  , testCase "error or 'v' - no input" $
      parse (constantParser UnexpectedEof ||| valueParser 'v') "" @?= Result "" 'v'
  , testCase "char or 'v' - have char" $
      parse (character ||| valueParser 'v') "abc" @?= Result "bc" 'a'
  , testCase "error or 'v' - have input" $
      parse (constantParser UnexpectedEof ||| valueParser 'v') "abc" @?= Result "abc" 'v'
  ]

listTest :: TestTree
listTest =
  testGroup "listTest" [
    testCase "no input" $
      parse (list character) "" @?= Result "" ""
  , testCase "" $
      parse (list digit) "123abc" @?= Result "abc" "123"
  , testCase "" $
      parse (list digit) "abc" @?= Result "abc" ""
  , testCase "" $
      parse (list character) "abc" @?= Result "" "abc"
  , testCase "" $
      parse (list (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
  , testCase "" $
      parse (list (character *> valueParser 'v')) "" @?= Result "" ""
  ]

list1Test :: TestTree
list1Test =
  testGroup "list1Test" [
  ]

satisfyTest :: TestTree
satisfyTest =
  testGroup "satisfyTest" [
  ]

sequenceParserTest :: TestTree
sequenceParserTest =
  testGroup "sequenceParserTest" [
  ]

thisManyTest :: TestTree
thisManyTest =
  testGroup "thisManyTest" [
  ]

ageParserTest :: TestTree
ageParserTest =
  testGroup "ageParserTest" [
  ]

firstNameParserTest :: TestTree
firstNameParserTest =
  testGroup "firstNameParserTest" [
  ]

surnameParserTest :: TestTree
surnameParserTest =
  testGroup "surnameParserTest" [
  ]

smokerParserTest :: TestTree
smokerParserTest =
  testGroup "smokerParserTest" [
  ]

phoneBodyParserTest :: TestTree
phoneBodyParserTest =
  testGroup "phoneBodyParserTest" [
  ]

phoneParserTest :: TestTree
phoneParserTest =
  testGroup "phoneParserTest" [
  ]

personParserTest :: TestTree
personParserTest =
  testGroup "personParserTest" [
  ]
