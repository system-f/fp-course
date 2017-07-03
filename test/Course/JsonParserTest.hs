{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.JsonParserTest where

import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))

import           Course.Core
import           Course.JsonParser (jsonString)
import           Course.Parser     (ParseResult (..), isErrorResult, parse)

test_JsonParser :: TestTree
test_JsonParser =
  testGroup "JsonParser" [
    jsonStringTest
  ]

jsonStringTest :: TestTree
jsonStringTest =
  testGroup "jsonString" [
    testCase "parse whole ASCII input" $
      parse jsonString "\" abc\"" @?= Result "" " abc"
  , testCase "parse only the first string of input" $
      parse jsonString "\"abc\"def" @?= Result "def" "abc"
  , testCase "parse back slash (\\)" $
      parse jsonString "\"\\babc\"def" @?= Result "def" "\babc"
  , testCase "parse unicode (\\u00abc)" $
      parse jsonString "\"\\u00abc\"def" @?= Result "def" "«c"
  , testCase "parse unicode (\\u00ff)" $
      parse jsonString "\"\\u00ffabc\"def" @?= Result "def" "ÿabc"
  , testCase "parse unicode (\\u00fa)" $
      parse jsonString "\"\\u00faabc\"def" @?= Result "def" "úabc"
  , testCase "parsing string without quotes is an error" $
      isErrorResult (parse jsonString "abc") @?= True
  , testCase "parsing string containing \\a is an error - \\a isn't a special character" $
      isErrorResult (parse jsonString "\"\\abc\"def") @?= True
  ]
