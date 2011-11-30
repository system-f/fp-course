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
  let e = oneof "\"\\/bfnrt" ||| hex
      c = (is '\\' >> e)
          ||| satisfyAll [(/= '"'), (/= '\\')]
  in betweenCharTok '"' '"' (list c)

-- Exercise 2
-- Parse a JSON rational.
-- ~~~ Use readSigned and readFloat ~~~
jsonNumber ::
  Parser Rational
jsonNumber =
  P (\i -> case readSigned readFloat i of
             [] -> Error ("Expected Rational but got " ++ show i)
             ((n, z):_) -> Value (z, n))

-- Exercise 3
-- Parse a JSON true literal.
-- ~~~ Use stringTok ~~~
jsonTrue ::
  Parser String
jsonTrue =
  stringTok "true"

-- Exercise 4
-- Parse a JSON false literal.
-- ~~~ Use stringTok ~~~
jsonFalse ::
  Parser String
jsonFalse =
  stringTok "false"

-- Exercise 5
-- Parse a JSON null literal.
-- ~~~ Use stringTok ~~~
jsonNull ::
  Parser String
jsonNull =
  stringTok "null"

-- Exercise 6
-- Parse a JSON array.
-- ~~~ Use betweenSepbyComma and jsonValue ~~~
jsonArray ::
  Parser [JsonValue]
jsonArray =
  betweenSepbyComma '[' ']' jsonValue

-- Exercise 7
-- Parse a JSON object.
-- ~~~ Use jsonString, charTok, betweenSepbyComma and jsonValue ~~~
jsonObject ::
  Parser Assoc
jsonObject =
  let field = (,) <$> (jsonString <* charTok ':') <*> jsonValue
  in betweenSepbyComma '{' '}' field

-- Exercise 8
-- Parse a JSON value.
-- ~~~ Use spaces, jsonNull, jsonTrue, jsonFalse, jsonArray, jsonString, jsonObject and jsonNumber ~~~
jsonValue ::
  Parser JsonValue
jsonValue =
      spaces *>
      (JsonNull <$ jsonNull
   ||| JsonTrue <$ jsonTrue
   ||| JsonFalse <$ jsonFalse
   ||| JsonArray <$> jsonArray
   ||| JsonString <$> jsonString
   ||| JsonObject <$> jsonObject
   ||| JsonRational False <$> jsonNumber)

-- Exercise 9
-- Read a file into a JSON value.
-- ~~~ Use readFile and jsonValue ~~~
readJsonValue ::
  FilePath
  -> IO JsonValue
readJsonValue p =
  do c <- readFile p
     case jsonValue <.> c of
       Error m -> error m
       Value a -> return a

