module L06.JsonParser where

import Numeric
import Control.Applicative
import L01.Validation
import L03.Parser
import L06.JsonValue
import L06.MoreParser

-- Exercise 1
-- Parse a JSON string. Handle double-quotes, control characters, hexadecimal characters.
-- ~~~ Use oneof, hex, is, satisfyAll, betweenCharTok, list ~~~
jsonString ::
  Parser String
jsonString =
  error "todo"

-- Exercise 2
-- Parse a JSON rational.
-- ~~~ Use readSigned and readFloat ~~~
jsonNumber ::
  Parser Rational
jsonNumber =
  error "todo"

-- Exercise 3
-- Parse a JSON true literal.
-- ~~~ Use stringTok ~~~
jsonTrue ::
  Parser String
jsonTrue =
  error "todo"

-- Exercise 4
-- Parse a JSON false literal.
-- ~~~ Use stringTok ~~~
jsonFalse ::
  Parser String
jsonFalse =
  error "todo"

-- Exercise 5
-- Parse a JSON null literal.
-- ~~~ Use stringTok ~~~
jsonNull ::
  Parser String
jsonNull =
  error "todo"

-- Exercise 6
-- Parse a JSON array.
-- ~~~ Use betweenSepbyComma and jsonValue ~~~
jsonArray ::
  Parser [JsonValue]
jsonArray =
  error "todo"

-- Exercise 7
-- Parse a JSON object.
-- ~~~ Use jsonString, charTok, betweenSepbyComma and jsonValue ~~~
jsonObject ::
  Parser Assoc
jsonObject =
  error "todo"

-- Exercise 8
-- Parse a JSON value.
-- ~~~ Use spaces, jsonNull, jsonTrue, jsonFalse, jsonArray, jsonString, jsonObject and jsonNumber ~~~
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

