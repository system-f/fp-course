module L05.Parser where

import Control.Applicative
import Data.Char
import L01.Validation
import L05.Person


type Input = String

data ParseResult a =
  UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | Failed
  | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show UnexpectedEof =
    "Expected end of stream"
  show (ExpectedEof i) =
    "Expected end of stream, but got >" ++ i ++ "<"
  show (UnexpectedChar c) =
    "Unexpected character" ++ [c]
  show Failed =
    "Parse failed"
  show (Result i a) =
    "Result >" ++ i ++ "< " ++ show a

-- Function to also access the input while binding parsers.
bindResult ::
  (Input -> a -> ParseResult b)
  -> ParseResult a
  -> ParseResult b
bindResult _ UnexpectedEof =
  UnexpectedEof
bindResult _ (ExpectedEof i) =
  ExpectedEof i
bindResult _ (UnexpectedChar c) =
  UnexpectedChar c
bindResult _ Failed =
  Failed
bindResult f (Result i a) =
  f i a

-- Function to determine is a parse result is an error.
isErrorResult ::
  ParseResult a
  -> Bool
isErrorResult UnexpectedEof =
  True
isErrorResult (ExpectedEof _) =
  True
isErrorResult (UnexpectedChar _) =
  True
isErrorResult Failed =
  True
isErrorResult (Result _ _) =
  False

data Parser a = P {
  parse :: Input -> ParseResult a
}

-- Exercise 1
-- | Return a parser that always succeeds with the given value and consumes no input.
--
-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3
valueParser :: a -> Parser a
valueParser = error "todo"

-- Exercise 2
-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse (failed "message") "abc")
-- True
failed :: Err -> Parser a
failed = error "todo"

-- Exercise 3
-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character :: Parser Char
character = error "todo"

-- Exercise 4
-- | Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--   * if that parser fails with an error the returned parser fails with that error.
-- ~~~ Use bindResult. ~~~
--
-- >>> parse (bindParser character (\c -> if c == 'x' then character else valueParser 'v')) "abc"
-- Result >bc< 'v'
--
-- >>> parse (bindParser character (\c -> if c == 'x' then character else valueParser 'v')) "a"
-- Result >< 'v'
--
-- >>> parse (bindParser character (\c -> if c == 'x' then character else valueParser 'v')) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (bindParser character (\c -> if c == 'x' then character else valueParser 'v')) "")
-- True
--
-- >>> isErrorResult (parse (bindParser character (\c -> if c == 'x' then character else valueParser 'v')) "x")
-- True
bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser = error "todo"

-- Exercise 5
-- | Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--   * if that parser fails with an error the returned parser fails with that error.
-- ~~~ This function should call bindParser. ~~~
--
-- >>> parse (character >>> valueParser 'v') "abc"
-- Result >bc< 'v'
--
-- >>> isErrorResult (parse (character >>> valueParser 'v') "")
-- True
(>>>) :: Parser a -> Parser b -> Parser b
(>>>) = error "todo"

-- Exercise 6
-- | Return a parser that tries the first parser for a successful value.
--   * If the first parser succeeds then use this parser.
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (failed ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (failed ||| valueParser 'v') "abc"
-- Result >abc< 'v'
(|||) :: Parser a -> Parser a -> Parser a
(|||) = error "todo"

infixl 3 |||

-- Exercise 7
-- | Return a parser that continues producing a list of values from the given parser.
-- ~~~ Use many1, valueParser and (|||). ~~~
--
-- >>> parse (list (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character >> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character >> valueParser 'v')) ""
-- Result >< ""
list :: Parser a -> Parser [a]
list = error "todo"

-- Exercise 8
-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
-- The returned parser fails if
--   * The input is empty
-- ~~~ Use bindParser, list and value. ~~~
--
--
-- >>> parse (many1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (many1 (character >> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (many1 (character >> valueParser 'v')) "")
-- True
many1 :: Parser a -> Parser [a]
many1 = error "todo"

-- Exercise 9
-- | Return a parser that produces a character but fails if
--   * The input is empty.
--   * The character does not satisfy the given predicate.
-- ~~~ The bindParser and character functions will be helpful here. ~~~
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy :: (Char -> Bool) -> Parser Char
satisfy = error "todo"

-- Exercise 10.1
-- Return a parser that produces the given character but fails if
--   * The input is empty.
--   * The produced character is not equal to the given character.
-- ~~~ Use the satisfy function. ~~~
is :: Char -> Parser Char
is = error "todo"

-- Exercise 10.2
-- Return a parser that produces a character between '0' and '9' but fails if
--   * The input is empty.
--   * The produced character is not a digit.
-- ~~~ Use the satisfy and Data.Char.isDigit functions. ~~~
digit :: Parser Char
digit = error "todo"

-- Exercise 10.3
-- Return a parser that produces zero or a positive integer but fails if
--   * The input is empty.
--   * The input does not produce a value series of digits
-- ~~~ Use the bindParser, valueParser, list and digit functions. ~~~
natural :: Parser Int
natural = error "todo"

-- Exercise 10.4
-- Return a parser that produces a space character but fails if
--   * The input is empty.
--   * The produced character is not a space.
-- ~~~ Use the satisfy and Data.Char.isSpace functions. ~~~
space :: Parser Char
space = error "todo"

-- Exercise 10.5
-- Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--   * The input is empty.
--   * The first produced character is not a space.
-- ~~~ Use the many1 and space functions. ~~~
spaces1 :: Parser String
spaces1 = error "todo"

-- Exercise 10.6
-- Return a parser that produces a lower-case character but fails if
--   * The input is empty.
--   * The produced character is not lower-case.
-- ~~~ Use the satisfy and Data.Char.isLower functions. ~~~
lower :: Parser Char
lower = error "todo"

-- Exercise 10.7
-- Return a parser that produces an upper-case character but fails if
--   * The input is empty.
--   * The produced character is not upper-case.
-- ~~~ Use the satisfy and Data.Char.isUpper functions. ~~~
upper :: Parser Char
upper = error "todo"

-- Exercise 10.8
-- Return a parser that produces an alpha character but fails if
--   * The input is empty.
--   * The produced character is not alpha.
-- ~~~ Use the satisfy and Data.Char.isAlpha functions. ~~~
alpha :: Parser Char
alpha = error "todo"

-- Exercise 11
-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
-- ~~~ Use bindParser and value. ~~~
-- ~~~ Optionally use Prelude.foldr. If not, an explicit recursive call. ~~~
--
-- >>> parse (sequenceParser [character, is 'x', upper]) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser [character, is 'x', upper]) "abCdef")
-- True
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser = error "todo"

-- Exercise 12
-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if
--   * The given parser fails in the attempt to produce the given number of values.
-- ~~~ Use sequenceParser and Prelude.replicate. ~~~
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany :: Int -> Parser a -> Parser [a]
thisMany = error "todo"

-- Exercise 13
-- | Write a parser for Person.age.
-- * Age: positive integer
-- ~~~ Equivalent to natural. ~~~
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True
ageParser :: Parser Int
ageParser = error "todo"

-- Exercise 14
-- | Write a parser for Person.firstName.
-- * First Name: non-empty string that starts with a capital letter
-- ~~~ Use bindParser, value, upper, list and lower. ~~~
--
-- λ> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- λ> isErrorResult (parse firstNameParser "abc")
-- True
firstNameParser :: Parser String
firstNameParser = error "todo"

-- Exercise 15
-- | Write a parser for Person.surname.
-- * Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters
-- ~~~ Use bindParser, value, upper, thisMany, lower and list. ~~~
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
surnameParser :: Parser String
surnameParser = error "todo"

-- Exercise 16
-- | Write a parser for Person.gender.
-- * Gender: character that must be 'm' or 'f'
-- ~~~ Use is and (|||). ~~~
--
-- >>> parse genderParser "mabc"
-- Result >abc< 'm'
--
-- >>> parse genderParser "fabc"
-- Result >abc< 'f'
--
-- >>> isErrorResult (parse genderParser "abc")
-- True
genderParser :: Parser Char
genderParser = error "todo"

-- Exercise 17
-- | Write part of a parser for Person.phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
-- * Phone: string of digits, dots or hyphens ...
-- ~~~ Use list, digit, (|||) and is. ~~~
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> parse phoneBodyParser "a123-456"
-- Result >a123-456< ""
phoneBodyParser :: Parser String
phoneBodyParser = error "todo"

-- Exercise 18
-- | Write a parser for Person.phone.
-- * Phone: ... but must start with a digit and end with a hash (#)
-- ~~~ Use bindParser, value, digit, phoneBodyParser and is. ~~~
--
-- >>> parse phoneParser "123-456#"
-- Result >< "123-456"
--
-- >>> parse phoneParser "123-456#abc"
-- Result >abc< "123-456"
--
-- >>> isErrorResult (parse phoneParser "123-456")
-- True
--
-- >>> isErrorResult (parse phoneParser "a123-456")
-- True
phoneParser :: Parser String
phoneParser = error "todo"

-- Exercise 19
-- Write a parser for Person.
--
-- | ~~~ Use bindParser, value, (>>>)
--         ageParser,
--         firstNameParser,
--         surnameParser,
--         genderParser,
--         phoneParser ~~~
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson m 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson m 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson m -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson m 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson m 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", gender = 'm', phone = "123-456.789"}
--
-- >>> parse personParser "123 Fred Clarkson m 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", gender = 'm', phone = "123-456.789"}
personParser :: Parser Person
personParser = error "todo"

-- Exercise 20
-- Make sure all the tests pass!


-- Exercise 20.1
-- Write a Functor instance for a Parser.
-- ~~~ Use bindParser and valueParser ~~~
instance Functor Parser where
  fmap =
    error "todo"

-- Exercise 20.2
-- Write an Applicative functor instance for a Parser.
-- ~~~ Use bindParser and valueParser ~~~
instance Applicative Parser where
  pure =
    error "todo"
  (<*>) =
    error "todo"

-- Exercise 20.3
-- Write a Monad instance for a Parser.
instance Monad Parser where
  return =
    error "todo"
  (>>=) =
    error "todo"
