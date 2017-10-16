{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ParserTest where

import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           Course.Applicative ((*>))
import           Course.Core
import           Course.List        (List ((:.), Nil))
import           Course.Parser      (ParseResult (Result, UnexpectedEof),
                                     ageParser, bindParser, character,
                                     constantParser, digit, firstNameParser, is,
                                     isErrorResult, list, list1, mapParser,
                                     parse, personParser, phoneBodyParser,
                                     phoneParser, satisfy, sequenceParser,
                                     smokerParser, surnameParser, thisMany,
                                     upper, valueParser, (>>>), (|||))
import           Course.Person      (Person (Person), age, firstName, phone,
                                     smoker, surname)

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
  testGroup "list" [
    testCase "no input" $
      parse (list character) "" @?= Result "" ""
  , testCase "digits" $
      parse (list digit) "123abc" @?= Result "abc" "123"
  , testCase "digits - no digits in input" $
      parse (list digit) "abc" @?= Result "abc" ""
  , testCase "characters" $
      parse (list character) "abc" @?= Result "" "abc"
  , testCase "characters replaced with 'v'" $
      parse (list (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
  , testCase "characters replaced with 'v' - no input" $
      parse (list (character *> valueParser 'v')) "" @?= Result "" ""
  ]

list1Test :: TestTree
list1Test =
  testGroup "list1" [
    testCase "one or more characters" $
      parse (list1 (character)) "abc" @?= Result "" "abc"
  , testCase "one or more characters replaced with 'v'" $
      parse (list1 (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
  , testCase "empty input is an error" $
      isErrorResult (parse (list1 (character *> valueParser 'v')) "") @?= True
  ]

satisfyTest :: TestTree
satisfyTest =
  testGroup "satisfy" [
    testCase "isUpper" $
      parse (satisfy isUpper) "Abc" @?= Result "bc" 'A'
  , testCase "isUpper with lower case input is an error" $
      isErrorResult (parse (satisfy isUpper) "abc") @?= True
  ]

sequenceParserTest :: TestTree
sequenceParserTest =
  testGroup "sequenceParser" [
    testCase "parser from a list of parsers" $
      parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef" @?= Result "def" "axC"
  , testCase "parser fails as expected" $
      isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef") @?= True
  ]

thisManyTest :: TestTree
thisManyTest =
  testGroup "thisMany" [
    testCase "4 uppers" $
      parse (thisMany 4 upper) "ABCDef" @?= Result "ef" "ABCD"
  , testCase "error on not enough uppers in input" $
      isErrorResult (parse (thisMany 4 upper) "ABcDef") @?= True
  ]

ageParserTest :: TestTree
ageParserTest =
  testGroup "ageParser" [
    testCase "120 works" $
      parse ageParser "120" @?= Result "" 120
  , testCase "non-digits are an error" $
      isErrorResult (parse ageParser "abc") @?= True
  , testCase "negative is an error" $
      isErrorResult (parse ageParser "-120") @?= True
  ]

firstNameParserTest :: TestTree
firstNameParserTest =
  testGroup "firstNameParser" [
    testCase "parses a first name" $
      parse firstNameParser "Abc" @?= Result "" "Abc"
  , testCase "error on lower case first letter" $
      isErrorResult (parse firstNameParser "abc") @?= True
  ]

surnameParserTest :: TestTree
surnameParserTest =
  testGroup "surnameParser" [
    testCase "parses a surname" $
      parse surnameParser "Abcdef" @?= Result "" "Abcdef"
  , testCase "parses a long surname" $
      parse surnameParser "Abcdefghijklmnopqrstuvwxyz" @?= Result "" "Abcdefghijklmnopqrstuvwxyz"
  , testCase "error on short surname" $
      isErrorResult (parse surnameParser "Abc") @?= True
  , testCase "error on lower case first letter" $
      isErrorResult (parse surnameParser "abcdef") @?= True
  ]

smokerParserTest :: TestTree
smokerParserTest =
  testGroup "smokerParser" [
    testCase "y" $
      parse smokerParser "yabc" @?= Result "abc" 'y'
  , testCase "n" $
      parse smokerParser "nabc" @?= Result "abc" 'n'
  , testCase "error when missing y/n" $
      isErrorResult (parse smokerParser "abc") @?= True
  ]

phoneBodyParserTest :: TestTree
phoneBodyParserTest =
  testGroup "phoneBodyParser" [
    testCase "numbers and dashes parsed" $
      parse phoneBodyParser "123-456" @?= Result "" "123-456"
  , testCase "parse up to first letter" $
      parse phoneBodyParser "123-4a56" @?= Result "a56" "123-4"
  , testCase "parse no input when input starts with letter" $
      parse phoneBodyParser "a123-456" @?= Result "a123-456" ""
  ]

phoneParserTest :: TestTree
phoneParserTest =
  testGroup "phoneParser" [
    testCase "whole input is phone number" $
      parse phoneParser "123-456#" @?= Result "" "123-456"
  , testCase "parse up to #" $
      parse phoneParser "123-456#abc" @?= Result "abc" "123-456"
  , testCase "error when no #" $
      isErrorResult (parse phoneParser "123-456") @?= True
  , testCase "error when starts with letter" $
      isErrorResult (parse phoneParser "a123-456") @?= True
  ]

personParserTest :: TestTree
personParserTest =
  testGroup "personParser" [
    testCase "error on empty" $
      isErrorResult (parse personParser "") @?= True
  , testCase "error on 'x' in age" $
      isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#") @?= True
  , testCase "error on lower case start of first name" $
      isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#") @?= True
  , testCase "error on short surname" $
      isErrorResult (parse personParser "123 Fred Cla y 123-456.789#") @?= True
  , testCase "error on lower case first in surname" $
      isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#") @?= True
  , testCase "error on 'x' for smoker" $
      isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#") @?= True
  , testCase "error on 'x' in phone" $
      isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#") @?= True
  , testCase "error on '-' at start of phone" $
      isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#") @?= True
  , testCase "error on missing '#' at end of phone" $
      isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789") @?= True
  , testCase "parse a valid person" $
      parse personParser "123 Fred Clarkson y 123-456.789#" @?= Result "" person
  , testCase "parse a valid person with leftover input" $
      parse personParser "123 Fred Clarkson y 123-456.789# rest" @?= Result " rest" person
  ]
  where person = Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
