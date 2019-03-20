{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.MoreParserTest (
  -- * Tests
    test_MoreParser
  , spacesTest
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

  -- * Course test runner
  , courseTest
  ) where

import           Test.Course.Mini  (courseTest)
import           Test.Mini         (MiniTestTree, assertBool, testCase,
                                    testGroup, (@?=))

import           Course.Core
import           Course.List       (List ((:.), Nil))
import           Course.MoreParser (between, betweenCharTok, betweenSepbyComma,
                                    charTok, commaTok, digits1, eof, hex, hexu,
                                    noneof, oneof, option, quote, satisfyAll,
                                    satisfyAny, sepby, sepby1, spaces, string,
                                    stringTok, tok)
import           Course.Parser     (ParseResult (Result), character, is,
                                    isErrorResult, lower, parse)

test_MoreParser :: MiniTestTree
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

spacesTest :: MiniTestTree
spacesTest =
  testGroup "spacesTest" [
      testCase "can parse zero spaces" $
        parse spaces "abc" @?= Result "abc" ""
    , testCase "can parse single space" $
        parse spaces " abc" @?= Result "abc" " "
    , testCase "can parse multiple spaces" $
        parse spaces "   abc" @?= Result "abc" "   "
  ]

tokTest :: MiniTestTree
tokTest =
  testGroup "tokTest" [
      testCase "can parse input without spaces" $
        parse (tok (is 'a')) "abc" @?= Result "bc" 'a'
    , testCase "can parse single space" $
        parse (tok (is 'a')) "a bc" @?= Result "bc" 'a'
    , testCase "can parse multiple spaces" $
        parse (tok (is 'a')) "a   bc" @?= Result "bc" 'a'
  ]

charTokTest :: MiniTestTree
charTokTest =
  testGroup "charTokTest" [
      assertBool "fails when character does not match" $
        isErrorResult (parse (charTok 'a') "dabc")
    , testCase "parses matching character" $
        parse (charTok 'a') "abc" @?= Result "bc" 'a'
    , testCase "parses matching character, dropping space" $
        parse (charTok 'a') "a bc" @?= Result "bc" 'a'
    , testCase "parses matching character, dropping spaces" $
        parse (charTok 'a') "a   bc" @?= Result "bc" 'a'
  ]

commaTokTest :: MiniTestTree
commaTokTest =
  testGroup "commaTokTest" [
      assertBool "fails when character is not a comma" $
        isErrorResult (parse commaTok "1,23")
    , testCase "parses leading comma" $
        parse commaTok ",123" @?= Result "123" ','
    , testCase "parses leading comma, dropping space" $
        parse commaTok ", 123" @?= Result "123" ','
    , testCase "parses leading comma, dropping multiple spaces" $
        parse commaTok ",   123" @?= Result "123" ','
  ]

quoteTest :: MiniTestTree
quoteTest =
  testGroup "quoteTest" [
      assertBool "fails when character is not a single or double quote" $
        isErrorResult (parse quote "abc")
    , testCase "parses single quote" $
        parse quote "'abc" @?= Result "abc" '\''
    , testCase "parses double quote" $
        parse quote "\"abc" @?= Result "abc" '"'
  ]

stringTest :: MiniTestTree
stringTest =
  testGroup "stringTest" [
      assertBool "fails when string is not matched" $
        isErrorResult (parse (string "abc") "bcdef")
    , testCase "parses matching string, leaves remaining input" $
        parse (string "abc") "abcdef" @?= Result "def" "abc"
    , testCase "parses matching string" $
        parse (string "abc") "abc" @?= Result "" "abc"
  ]

stringTokTest :: MiniTestTree
stringTokTest =
  testGroup "stringTokTest" [
      assertBool "fails when string is not matched" $
        isErrorResult (parse (stringTok "abc") "bc  ")
    , testCase "parses matching string followed by zero spaces" $
        parse (stringTok "abc") "abc" @?= Result "" "abc"
    , testCase "parses matching string followed by many spaces" $
        parse (stringTok "abc") "abc  " @?= Result "" "abc"
  ]

optionTest :: MiniTestTree
optionTest =
  testGroup "optionTest" [
      testCase "produces parsed value when parser succeeds" $
        parse (option 'x' character) "abc" @?= Result "bc" 'a'
    , testCase "produces given value when parser fails" $
        parse (option 'x' character) "" @?= Result "" 'x'
  ]

digits1Test :: MiniTestTree
digits1Test =
  testGroup "digits1Test" [
      assertBool "fails when no digits at start of input" $
        isErrorResult (parse digits1 "abc123")
    , testCase "succeeds on digits" $
        parse digits1 "123" @?= Result "" "123"
    , testCase "succeeds on digits, leaves remaining input" $
        parse digits1 "123abc" @?= Result "abc" "123"
  ]

oneofTest :: MiniTestTree
oneofTest =
  testGroup "oneofTest" [
      assertBool "fails when given character not in string" $
        isErrorResult (parse (oneof "abc") "def")
    , testCase "given character prefixes input" $
        parse (oneof "abc") "bcdef" @?= Result "cdef" 'b'
  ]

noneofTest :: MiniTestTree
noneofTest =
  testGroup "noneofTest" [
      assertBool "fails when one of given characters prefixes input" $
        isErrorResult (parse (noneof "abcd") "abc")
    , testCase "succeeds when none of the given characters in input" $
        parse (noneof "xyz") "abc" @?= Result "bc" 'a'
    , testCase "succeeds when none of the given characters prefixes input" $
        parse (noneof "bcd") "abc" @?= Result "bc" 'a'
  ]

betweenTest :: MiniTestTree
betweenTest =
  testGroup "betweenTest" [
      assertBool "fails when opening parse fails" $
        isErrorResult (parse (between (is '[') (is ']') character) "abc]")
   ,  assertBool "fails when surrounded parser fails" $
        isErrorResult (parse (between (is '[') (is ']') character) "[abc]")
   ,  assertBool "fails when closing parse fails" $
        isErrorResult (parse (between (is '[') (is ']') character) "[abc")
    , testCase "succeeds: character surrounded by []'" $
        parse (between (is '[') (is ']') character) "[a]" @?= Result "" 'a'
    , testCase "succeeds: digits surrounded by []" $
        parse (between (is '[') (is ']') digits1) "[123]" @?= Result "" "123"
  ]

betweenCharTokTest :: MiniTestTree
betweenCharTokTest =
  testGroup "betweenCharTokTest" [
      assertBool "fails when opening character not present" $
        isErrorResult (parse (betweenCharTok '[' ']' character) "abc]")
    , assertBool "fails when closing character not present" $
        isErrorResult (parse (betweenCharTok '[' ']' character) "[abc")
   ,  assertBool "fails when surrounded parser fails" $
        isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]")
    , testCase "succeeds: character" $
        parse (betweenCharTok '[' ']' character) "[a]" @?= Result "" 'a'
    , testCase "succeeds: digits1" $
        parse (betweenCharTok '[' ']' digits1) "[123]" @?= Result "" "123"
  ]

hexTest :: MiniTestTree
hexTest =
  testGroup "hexTest" [
      assertBool "fails on invalid hex string --- too short" $
        isErrorResult (parse hex "001")
    , assertBool "fails on invalid hex string --- invalid char (x)" $
        isErrorResult (parse hex "0axf")
    , testCase "succeeds on valid hex value" $
        parse hex "0010" @?= Result "" '\DLE'
  ]

hexuTest :: MiniTestTree
hexuTest =
  testGroup "hexuTest" [
      assertBool "fails when no u at start" $
        isErrorResult (parse hexu "0010")
   ,  assertBool "fails when not 4 hex digits after u" $
        isErrorResult (parse hexu "u010")
   ,  assertBool "fails on invalid hex digit" $
        isErrorResult (parse hexu "u0axf")
    , testCase "succeeds on valid input --- u0010" $
        parse hexu "u0010" @?= Result "" '\DLE'
    , testCase "succeeds on valid input --- u0a1f" $
        parse hexu "u0a1f" @?= Result "" '\2591'
  ]

sepby1Test :: MiniTestTree
sepby1Test =
  testGroup "sepby1Test" [
      assertBool "fails when first parser fails" $
        isErrorResult (parse (sepby1 character (is ',')) "")
    , testCase "parses single character not followed by seperator" $
        parse (sepby1 character (is ',')) "a" @?= Result "" "a"
    , testCase "parses multiple matches with separators" $
        parse (sepby1 character (is ',')) "a,b,c" @?= Result "" "abc"
    , testCase "succeeds until two separators" $
        parse (sepby1 character (is ',')) "a,b,c,,def" @?= Result "def" "abc,"
  ]

sepbyTest :: MiniTestTree
sepbyTest =
  testGroup "sepbyTest" [
      testCase "succeeds on empty string" $
        parse (sepby character (is ',')) "" @?= Result "" ""
    , testCase "succeeds on single match without seperator" $
        parse (sepby character (is ',')) "a" @?= Result "" "a"
    , testCase "succeeds on multiple matches with seperators" $
        parse (sepby character (is ',')) "a,b,c" @?= Result "" "abc"
    , testCase "succeeds until two separators" $
        parse (sepby character (is ',')) "a,b,c,,def" @?= Result "def" "abc,"
  ]

eofTest :: MiniTestTree
eofTest =
  testGroup "eofTest" [
      assertBool "fails when still input left" $
        isErrorResult (parse eof "abc")
    , testCase "succeeds when no input left" $
        parse eof "" @?= Result "" ()
  ]

satisfyAllTest :: MiniTestTree
satisfyAllTest =
  testGroup "satisfyAllTest" [
      assertBool "fails when a predicate fails" $
        isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "XBc")
    , assertBool "fails when no predicates satisfied (empty input)" $
        isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "")
    , assertBool "fails when no predicates satisfied" $
        isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "abc")
    , testCase "succeeds when all predicates satisfied: ABC" $
        parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABC" @?= Result "BC" 'A'
    , testCase "succeeds when all predicates satisfied: ABc" $
        parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABc" @?= Result "Bc" 'A'
  ]

satisfyAnyTest :: MiniTestTree
satisfyAnyTest =
  testGroup "satisfyAnyTest" [
      assertBool "fails when no predicates satisfied" $
        isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "XBc")
    , assertBool "fails when no predicates satisfied (empty input)" $
        isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "")
    , testCase "succeeds when all predicates satisfied" $
        parse (satisfyAny (isUpper :. (/= 'X') :. Nil)) "ABc" @?= Result "Bc" 'A'
    , testCase "succeeds when one of two predicates satisfied" $
        parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "ABc" @?= Result "Bc" 'A'
  ]

betweenSepbyCommaTest :: MiniTestTree
betweenSepbyCommaTest =
  testGroup "betweenSepbyCommaTest" [
      assertBool "fails when opening char missing" $
        isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]")
    , assertBool "fails when closing char missing" $
        isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a")
    , assertBool "fails when input between seperators doesn't match (multiple matches)" $
        isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]")
    , assertBool "fails when input between seperators doesn't match" $
        isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]")
    , testCase "succeeds --- one match" $
        parse (betweenSepbyComma '[' ']' lower) "[a]" @?= Result "" "a"
    , testCase "succeeds --- nothing between surrounds" $
        parse (betweenSepbyComma '[' ']' lower) "[]" @?= Result "" ""
    , testCase "succeeds --- 3 matches" $
        parse (betweenSepbyComma '[' ']' lower) "[a,b,c]" @?= Result "" "abc"
    , testCase "succeeds --- 3 padded matches" $
        parse (betweenSepbyComma '[' ']' lower) "[a,  b, c]" @?= Result "" "abc"
    , testCase "succeeds --- digits1" $
        parse (betweenSepbyComma '[' ']' digits1) "[123,456]" @?= Result "" ("123":."456":.Nil)
  ]

