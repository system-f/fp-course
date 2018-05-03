{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.JsonParserTest where

import           Data.Ratio        ((%))
import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))

import           Course.Core
import           Course.JsonParser (jsonArray, jsonFalse, jsonNull, jsonNumber,
                                    jsonObject, jsonString, jsonTrue, jsonValue)
import           Course.JsonValue  (JsonValue (..))
import           Course.List       (List (..))
import           Course.Parser     (ParseResult (..), isErrorResult, parse)

test_JsonParser :: TestTree
test_JsonParser =
  testGroup "JsonParser" [
    jsonStringTest
  , jsonNumberTest
  , jsonTrueTest
  , jsonFalseTest
  , jsonNullTest
  , jsonArrayTest
  , jsonObjectTest
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

jsonNumberTest :: TestTree
jsonNumberTest =
  testGroup "jsonNumber" [
    testCase "positive whole" $ parse jsonNumber "234" @?= Result "" (234 % 1)
  , testCase "negative whole" $ parse jsonNumber "-234" @?= Result "" ((-234) % 1)
  , testCase "positive decimal" $ parse jsonNumber "123.45" @?= Result "" (2469 % 20)
  , testCase "negative whole (2)" $ parse jsonNumber "-123" @?= Result "" ((-123) % 1)
  , testCase "negative decimal" $ parse jsonNumber "-123.45" @?= Result "" ((-2469) % 20)
  , testCase "negative sign on its own is error" $ isErrorResult (parse jsonNumber "-") @?= True
  , testCase "alphabetic characters is error" $ isErrorResult (parse jsonNumber "abc") @?= True
  ]

jsonTrueTest :: TestTree
jsonTrueTest =
  testGroup "jsonTrue" [
    testCase "parses true" $ parse jsonTrue "true" @?= Result "" "true"
  , testCase "TRUE (caps) is an error" $ isErrorResult (parse jsonTrue "TRUE") @?= True
  ]

jsonFalseTest :: TestTree
jsonFalseTest =
  testGroup "jsonFalse" [
    testCase "parses false" $ parse jsonFalse "false" @?= Result "" "false"
  , testCase "FALSE (caps) is an error" $ isErrorResult (parse jsonFalse "FALSE") @?= True
  ]

jsonNullTest :: TestTree
jsonNullTest =
  testGroup "jsonNull" [
    testCase "parses null" $ parse jsonNull "null" @?= Result "" "null"
  , testCase "NULL (caps) is an error" $ isErrorResult (parse jsonNull "NULL") @?= True
  ]

jsonArrayTest :: TestTree
jsonArrayTest =
  testGroup "jsonArray" [
    testCase "[]" $
      parse jsonArray "[]" @?= Result "" Nil
  , testCase "[true]" $
      parse jsonArray "[true]" @?= Result "" (JsonTrue :. Nil)
  , testCase "[true, \"abc\"]" $
      parse jsonArray "[true, \"abc\"]" @?= Result "" (JsonTrue :. JsonString "abc" :. Nil)
  , testCase "[true, \"abc\", []]" $
      parse jsonArray "[true, \"abc\", []]" @?= Result "" (JsonTrue :. JsonString "abc" :. JsonArray Nil :. Nil)
  , testCase "[true, \"abc\", [false]]" $
      let result = Result "" (JsonTrue :. JsonString "abc" :. JsonArray (JsonFalse :. Nil) :. Nil)
       in parse jsonArray "[true, \"abc\", [false]]" @?= result
    ]

jsonObjectTest :: TestTree
jsonObjectTest =
  testGroup "jsonObject" [
    testCase "empty" $
      parse jsonObject "{}" @?= Result "" Nil
  , testCase "one key" $
      parse jsonObject "{ \"key1\" : true }" @?= Result "" (("key1",JsonTrue) :. Nil)
  , testCase "two keys" $
      parse jsonObject "{ \"key1\" : true , \"key2\" : false }" @?= Result "" (("key1",JsonTrue):.("key2",JsonFalse):.Nil)
  , testCase "two keys and left over input" $
      let result = Result "xyz" (("key1",JsonTrue):.("key2",JsonFalse):.Nil)
       in parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz" @?= result
  ]

jsonValueTest :: TestTree
jsonValueTest =
  testGroup "jsonValue" [
    testCase "true" $
      parse jsonValue "true" @?= Result "" JsonTrue
  , testCase "object" $
      let result = Result "" (  ("key1",JsonTrue)
                             :. ("key2",JsonArray (JsonRational (7 % 1) :. JsonFalse:.Nil))
                             :. Nil
                             )
       in parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }" @?= result
  , testCase "nested object" $
      let result = Result "" (  ("key1",JsonTrue)
                             :. ("key2",JsonArray (JsonRational (7 % 1) :. JsonFalse :. Nil))
                             :. ("key3",JsonObject (("key4",JsonNull) :. Nil))
                             :. Nil
                             )
       in parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }" @?= result
  ]
