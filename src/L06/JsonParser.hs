module L06.JsonParser where

import Numeric
import Control.Applicative
import L01.Validation
import L05.Parser
import L06.JsonValue
import L06.MoreParser

-- Exercise 1
-- | Parse a JSON string. Handle double-quotes, control characters, hexadecimal characters.
-- ~~~ Use oneof, hex, is, satisfyAll, betweenCharTok, list ~~~
--
-- >>> parse jsonString "\"abc\""
-- Value ("","abc")
--
-- >>> parse jsonString "\"abc\"def"
-- Value ("def","abc")
--
-- >>> parse jsonString "\"\\babc\"def"
-- Value ("def","babc")
--
-- >>> parse jsonString "\"\\u00abc\"def"
-- Value ("def","\171c")
--
-- >>> parse jsonString "\"\\u00ffabc\"def"
-- Value ("def","\255abc")
--
-- >>> parse jsonString "\"\\u00faabc\"def"
-- Value ("def","\250abc")
--
-- >>> isError (parse jsonString "abc")
-- True
--
-- >>> isError (parse jsonString "\"\\abc\"def")
-- True
jsonString ::
  Parser String
jsonString =
  error "todo"

-- Exercise 2
-- | Parse a JSON rational.
-- ~~~ Use Numeric#readSigned and Numeric#readFloat ~~~
--
-- >>> parse jsonNumber "234"
-- Value ("",234 % 1)
--
-- >>> parse jsonNumber "-234"
-- Value ("",(-234) % 1)
--
-- >>> parse jsonNumber "123.45"
-- Value ("",2469 % 20)
--
-- >>> parse jsonNumber "-123"
-- Value ("",(-123) % 1)
--
-- >>> parse jsonNumber "-123.45"
-- Value ("",(-2469) % 20)
--
-- >>> isError (parse jsonNumber "-")
-- True
--
-- >>> isError (parse jsonNumber "abc")
-- True
jsonNumber ::
  Parser Rational
jsonNumber =
  error "todo"

-- Exercise 3
-- | Parse a JSON true literal.
-- ~~~ Use stringTok ~~~
--
-- >>> parse jsonTrue "true"
-- Value ("","true")
--
-- >>> isError (parse jsonTrue "TRUE")
-- True
jsonTrue ::
  Parser String
jsonTrue =
  error "todo"

-- Exercise 4
-- | Parse a JSON false literal.
-- ~~~ Use stringTok ~~~
--
--
-- >>> parse jsonFalse "false"
-- Value ("","false")
--
-- >>> isError (parse jsonFalse "FALSE")
-- True
jsonFalse ::
  Parser String
jsonFalse =
  error "todo"

-- Exercise 5
-- | Parse a JSON null literal.
-- ~~~ Use stringTok ~~~
--
-- >>> parse jsonNull "null"
-- Value ("","null")
--
-- >>> isError (parse jsonNull "NULL")
-- True
jsonNull ::
  Parser String
jsonNull =
  error "todo"

-- Exercise 6
-- | Parse a JSON array.
-- ~~~ Use betweenSepbyComma and jsonValue ~~~
--
-- >>> parse jsonArray "[]"
-- Value ("",[])
--
-- >>> parse jsonArray "[true]"
-- Value ("",[JsonTrue])
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Value ("",[JsonTrue,JsonString "abc"])
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Value ("",[JsonTrue,JsonString "abc",JsonArray []])
--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Value ("",[JsonTrue,JsonString "abc",JsonArray [JsonFalse]])
jsonArray ::
  Parser [JsonValue]
jsonArray =
  error "todo"

-- Exercise 7
-- | Parse a JSON object.
-- ~~~ Use jsonString, charTok, betweenSepbyComma and jsonValue ~~~
--
-- >>> parse jsonObject "{}"
-- Value ("",[])
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Value ("",[("key1",JsonTrue)])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Value ("",[("key1",JsonTrue),("key2",JsonFalse)])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Value ("xyz",[("key1",JsonTrue),("key2",JsonFalse)])
jsonObject ::
  Parser Assoc
jsonObject =
  error "todo"

-- Exercise 8
-- | Parse a JSON value.
-- ~~~ Use spaces, jsonNull, jsonTrue, jsonFalse, jsonArray, jsonString, jsonObject and jsonNumber ~~~
--
-- >>> parse jsonValue "true"
-- Value ("",JsonTrue)
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Value ("",[("key1",JsonTrue),("key2",JsonArray [JsonRational False (7 % 1),JsonFalse])])
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Value ("",[("key1",JsonTrue),("key2",JsonArray [JsonRational False (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])])
jsonValue ::
  Parser JsonValue
jsonValue =
  error "todo"

-- Exercise 9
-- Read a file into a JSON value.
-- ~~~ Use readFile and jsonValue ~~~
readJsonValue ::
  FilePath
  -> IO JsonValue
readJsonValue =
  error "todo"

