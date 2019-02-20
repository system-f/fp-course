{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.MoreParserTest where

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=), assertBool)

import           Course.Core
import           Course.List           (List (..))
import           Course.Parser
import           Course.MoreParser

test_MoreParser :: TestTree
test_MoreParser =
  testGroup "MoreParser" [
      spacesTest
    , tokTest
    , charTokTest
    , commaTokTest
    , quoteTest
    , stringTest
    , stringTokTest
    , optionTest
    , digits1Test
    , oneofTest
    , noneofTest
    , betweenTest
    , betweenCharTokTest
    , hexTest
    , hexuTest
    , sepby1Test
    , sepbyTest
    , eofTest
    , satisfyAllTest
    , satisfyAnyTest
    , betweenSepbyCommaTest
  ]

spacesTest :: TestTree
spacesTest =
  testGroup "spacesTest" [
      testCase "can parse zero spaces" $
        parse spaces "abc" @?= Result "abc" ""
    , testCase "can parse single space" $
        parse spaces " abc" @?= Result "abc" " "
    , testCase "can parse multiple spaces" $
        parse spaces "   abc" @?= Result "abc" "   "
  ]

tokTest :: TestTree
tokTest =
  testGroup "tokTest" [
      testCase "can parse input without spaces" $
        parse (tok (is 'a')) "abc" @?= Result "bc" 'a'
    , testCase "can parse single space" $
        parse (tok (is 'a')) "a bc" @?= Result "bc" 'a'
    , testCase "can parse multiple spaces" $
        parse (tok (is 'a')) "a   bc" @?= Result "bc" 'a'
  ]

charTokTest :: TestTree
charTokTest =
  testGroup "charTokTest" [
      testCase "fails when character does not match" $
        assertBool "fails when character does not match" $
          isErrorResult (parse (charTok 'a') "dabc")
    , testCase "parses successfully when character matches" $ do
        parse (charTok 'a') "abc" @?= Result "bc" 'a'
        parse (charTok 'a') "a bc" @?= Result "bc" 'a'
        parse (charTok 'a') "a   bc" @?= Result "bc" 'a'
  ]

commaTokTest :: TestTree
commaTokTest =
  testGroup "commaTokTest" [
      testCase "fails when character is not a comma" $
        assertBool "fails when character is not a comma" $
          isErrorResult (parse commaTok "1,23")
    , testCase "parses successfully when character is a comma" $ do
        parse commaTok ",123" @?= Result "123" ','
        parse commaTok ", 123" @?= Result "123" ','
        parse commaTok ",   123" @?= Result "123" ','
  ]

quoteTest :: TestTree
quoteTest =
  testGroup "quoteTest" [
      testCase "fails when character is not a single or double quote" $
        assertBool "fails when character is not a single or double quote" $
          isErrorResult (parse quote "abc")
    , testCase "parses single quotes" $ do
        parse quote "'abc" @?= Result "abc" '\''
        parse quote "\"abc" @?= Result "abc" '"'
  ]

stringTest :: TestTree
stringTest =
  testGroup "stringTest" [
      testCase "fails when string is not matched" $
        assertBool "fails when string is not matched" $
          isErrorResult (parse (string "abc") "bcdef")
    , testCase "parses string that matches" $ do
        parse (string "abc") "abcdef" @?= Result "def" "abc"
        parse (string "abc") "abc" @?= Result "" "abc"
  ]

stringTokTest :: TestTree
stringTokTest =
  testGroup "stringTokTest" [
      testCase "fails when string is not matched" $
        assertBool "fails when string is not matched" $
          isErrorResult (parse (stringTok "abc") "bc  ")
    , testCase "parses matching string followed by zero or more spaces" $ do
        parse (stringTok "abc") "abc" @?= Result "" "abc"
        parse (stringTok "abc") "abc " @?= Result "" "abc"
        parse (stringTok "abc") "abc " @?= Result "" "abc"
  ]

optionTest :: TestTree
optionTest =
  testGroup "optionTest" [
      testCase "produces parsed value when parser fails" $
        parse (option 'x' character) "abc" @?= Result "bc" 'a'
    , testCase "produces given value when parser fails" $
        parse (option 'x' character) "" @?= Result "" 'x'
  ]

digits1Test :: TestTree
digits1Test =
  testGroup "digits1Test" [
      testCase "fails when no digits at start of input" $
        assertBool "fails when no digits at start of input" $
          isErrorResult (parse digits1 "abc123")
    , testCase "succeeds when there are digits at start of input" $ do
        parse digits1 "123" @?= Result "" "123"
        parse digits1 "123abc" @?= Result "abc" "123"
  ]

oneofTest :: TestTree
oneofTest =
  testGroup "oneofTest" [
      testCase "fails when given character not in string" $
        assertBool "fails when given character not in string" $
          isErrorResult (parse (oneof "abc") "def")
    , testCase "succeeds when there are digits at start of input" $
        parse (oneof "abc") "bcdef" @?= Result "cdef" 'b'
  ]

noneofTest :: TestTree
noneofTest =
  testGroup "noneofTest" [
      testCase "fails when any character at start of input" $
        assertBool "fails when any character at start of input" $
          isErrorResult (parse (noneof "abcd") "abc")
    , testCase "succeeds when there are digits at start of input" $ do
        parse (noneof "xyz") "abc" @?= Result "bc" 'a'
        parse (noneof "bcd") "abc" @?= Result "bc" 'a'
  ]

betweenTest :: TestTree
betweenTest =
  testGroup "betweenTest" [
      testCase "fails when sequence can't be parsed" $ do
        assertBool "should fail when first parser fails" $
          isErrorResult (parse (between (is '[') (is ']') character) "abc]")
        assertBool "should fail when second parser fails" $
          isErrorResult (parse (between (is '[') (is ']') character) "[abc]")
        assertBool "should fail when third parser fails" $
          isErrorResult (parse (between (is '[') (is ']') character) "[abc")
    , testCase "succeeds when all three parsers succeed" $ do
        parse (between (is '[') (is ']') character) "[a]" @?= Result "" 'a'
        parse (between (is '[') (is ']') digits1) "[123]" @?= Result "" "123"
  ]

betweenCharTokTest :: TestTree
betweenCharTokTest =
  testGroup "betweenCharTokTest" [
      testCase "fails when sequence can't be parsed" $ do
        assertBool "should fail when opening char not parsed" $
          isErrorResult (parse (betweenCharTok '[' ']' character) "abc]")
        assertBool "should fail when closing char not parsed" $
          isErrorResult (parse (betweenCharTok '[' ']' character) "[abc")
        assertBool "should fail when given parser fails" $
          isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]")
    , testCase "succeeds when sequence can be parsed" $ do
        parse (betweenCharTok '[' ']' character) "[a]" @?= Result "" 'a'
        parse (betweenCharTok '[' ']' digits1) "[123]" @?= Result "" "123"
  ]

hexTest :: TestTree
hexTest =
  testGroup "hexTest" [
      testCase "fails on invalid hex string" $ do
        assertBool "fails on invalid hex string" $
          isErrorResult (parse hex "001")
        assertBool "fails on invalid hex string" $
          isErrorResult (parse hex "0axf")
    , testCase "succeeds on valid hex value" $ do
        parse hex "0010" @?= Result "" '\DLE'
  ]

hexuTest :: TestTree
hexuTest =
  testGroup "hexuTest" [
      testCase "fails on invalid string" $ do
        assertBool "fails when no u at start" $
          isErrorResult (parse hexu "0010")
        assertBool "fails when not 4 hex digits after u" $
          isErrorResult (parse hexu "u010")
        assertBool "fails on invalid hex" $
          isErrorResult (parse hexu "u0axf")
    , testCase "succeeds on valid string" $ do
        parse hexu "u0010" @?= Result "" '\DLE'
        parse hexu "u0a1f" @?= Result "" '\2591'
  ]

sepby1Test :: TestTree
sepby1Test =
  testGroup "sepby1Test" [
      testCase "fails when first parser fails" $ do
        assertBool "fails when first parser fails" $
          isErrorResult (parse (sepby1 character (is ',')) "")
    , testCase "parses single character not followed by seperator" $
        parse (sepby1 character (is ',')) "a" @?= Result "" "a"
    , testCase "parses multiple characters with seperator inbetween" $ do
        parse (sepby1 character (is ',')) "a,b,c" @?= Result "" "abc"
        parse (sepby1 character (is ',')) "a,b,c,,def" @?= Result "def" "abc,"
  ]

sepbyTest :: TestTree
sepbyTest =
  testGroup "sepbyTest" [
      testCase "succeeds on empty string" $
        parse (sepby character (is ',')) "" @?= Result "" ""
    , testCase "succeeds on single match without seperator" $
        parse (sepby character (is ',')) "a" @?= Result "" "a"
    , testCase "succeeds on multiple matches with seperator inbetween" $
        parse (sepby character (is ',')) "a,b,c" @?= Result "" "abc"
    , testCase "succeeds up until first character fails" $
        parse (sepby character (is ',')) "a,b,c,,def" @?= Result "def" "abc,"
  ]

eofTest :: TestTree
eofTest =
  testGroup "eofTest" [
      testCase "fails when still input left" $ do
        assertBool "fails when still input left" $
          isErrorResult (parse eof "abc")
    , testCase "succeeds when no input left" $
        parse eof "" @?= Result "" ()
  ]

satisfyAllTest :: TestTree
satisfyAllTest =
  testGroup "satisfyAllTest" [
      testCase "fails when not all of the predicates pass" $ do
        assertBool "fails not all of the predicates pass" $
          isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "XBc")
    , testCase "fails when none of the predicates pass" $ do
        assertBool "fails when none of the predicates pass" $
          isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "")
        assertBool "fails when none of the predicates pass" $
          isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "abc")
    , testCase "succeeds when all predicats pass" $ do
        parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABC" @?= Result "BC" 'A'
        parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABc" @?= Result "Bc" 'A'
  ]

satisfyAnyTest :: TestTree
satisfyAnyTest =
  testGroup "satisfyAnyTest" [
      testCase "fails when none of the predicates pass" $ do
        assertBool "fails when none of the predicates pass" $
          isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "XBc")
        assertBool "fails when none of the predicates pass" $
          isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "")
    , testCase "succeeds when all predicates pass" $
        parse (satisfyAny (isUpper :. (/= 'X') :. Nil)) "ABc" @?= Result "Bc" 'A'
    , testCase "succeeds whan any predicate passes" $
        parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "ABc" @?= Result "Bc" 'A'
  ]

betweenSepbyCommaTest :: TestTree
betweenSepbyCommaTest =
  testGroup "betweenSepbyCommaTest" [
      testCase "fails on invalid inputs" $ do
        assertBool "fails when opening char missing" $
          isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]")
        assertBool "fails when closing char missing" $
          isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a")
        assertBool "fails when input between seperators doesn't match" $
          isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]")
        assertBool "fails when input between seperators doesn't match" $
          isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]")
    , testCase "succeeds on valid input" $ do
        parse (betweenSepbyComma '[' ']' lower) "[a]" @?= Result "" "a"
        parse (betweenSepbyComma '[' ']' lower) "[]" @?= Result "" ""
        parse (betweenSepbyComma '[' ']' lower) "[a,b,c]" @?= Result "" "abc"
        parse (betweenSepbyComma '[' ']' lower) "[a,  b, c]" @?= Result "" "abc"
        parse (betweenSepbyComma '[' ']' digits1) "[123,456]" @?= Result "" ("123":."456":.Nil)
  ]

