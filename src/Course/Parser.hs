{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Parser where

import Course.Core
import Data.Char
import Course.Person
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.List
import Course.Optional
import qualified Prelude as P

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Str

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
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character", show [c]]
  show Failed =
    "Parse failed"
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]

-- Function to also access the input while binding parsers.
withResultInput ::
  (Input -> a -> ParseResult b)
  -> ParseResult a
  -> ParseResult b
withResultInput _ UnexpectedEof =
  UnexpectedEof
withResultInput _ (ExpectedEof i) =
  ExpectedEof i
withResultInput _ (UnexpectedChar c) =
  UnexpectedChar c
withResultInput _ Failed =
  Failed
withResultInput f (Result i a) =
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
valueParser ::
  a
  -> Parser a
valueParser a =
  P (\i -> Result i a)

-- Exercise 2
-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse failed "abc")
-- True
failed ::
  Parser a
failed =
  P (\_ -> Failed)

-- Exercise 3
-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character ::
  Parser Char
character =
  P (\s -> case s of Nil -> UnexpectedEof
                     (c:.r) -> Result r c)

-- Exercise 4
-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @withResultInput@.
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "abc"
-- Result >bc< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "a"
-- Result >< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "")
-- True
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "x")
-- True
bindParser ::
  (a -> Parser b)
  -> Parser a
  -> Parser b
bindParser f (P p) =
  P (withResultInput (\r c -> parse (f c) r) . p)

fbindParser ::
  Parser a
  -> (a -> Parser b)
  -> Parser b
fbindParser =
  flip bindParser

-- Exercise 5
-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @bindParser@.
--
-- >>> parse (character >>> valueParser 'v') "abc"
-- Result >bc< 'v'
--
-- >>> isErrorResult (parse (character >>> valueParser 'v') "")
-- True
(>>>) ::
  Parser a
  -> Parser b
  -> Parser b
p >>> q =
  bindParser (\_ -> q) p

-- Exercise 6
-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
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
(|||) ::
  Parser a
  -> Parser a
  -> Parser a
P p1 ||| P p2 =
  P (\s -> let v = p1 s
           in if isErrorResult v
                then
                  p2 s
                else
                  v)

infixl 3 |||

-- Exercise 7
-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @many1@, @valueParser@ and @(|||)@.
--
-- >>> parse (list (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> valueParser 'v')) ""
-- Result >< ""
list ::
  Parser a
  -> Parser (List a)
list k =
  many1 k ||| valueParser Nil

-- Exercise 8
-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
-- The returned parser fails if The input is empty.
--
-- /Tip:/ Use @bindParser@, @list@ and @value@.
--
-- >>> parse (many1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (many1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (many1 (character *> valueParser 'v')) "")
-- True
many1 ::
  Parser a
  -> Parser (List a)
many1 k =
  fbindParser k (\k' ->
  fbindParser (list k) (\kk' ->
  valueParser (k' :. kk')))

-- Exercise 9
-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @bindParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy ::
  (Char -> Bool)
  -> Parser Char
satisfy p =
  bindParser (\c ->
    if p c then valueParser c else failed) character

-- Exercise 10.1
-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
is ::
  Char -> Parser Char
is c =
  satisfy (== c)

-- Exercise 10.2
-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isDigit@ functions.
digit ::
  Parser Char
digit =
  satisfy isDigit

-- Exercise 10.3
-- | Return a parser that produces zero or a positive integer but fails if
--
--   * The input is empty.
--
--   * The input does not produce a value series of digits
--
-- /Tip:/ Use the @bindParser@, @valueParser@, @list@, @reads@ and @digit@
-- functions.
natural ::
  Parser Int
natural =
  bindParser (\k -> case read k of Empty        -> failed
                                   Full h -> valueParser h) (list digit)

-- Exercise 10.4
--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isSpace@ functions.
space ::
  Parser Char
space =
  satisfy isSpace

-- Exercise 10.5
-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @many1@ and @space@ functions.
spaces1 ::
  Parser Str
spaces1 =
  many1 space

-- Exercise 10.6
-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isLower@ functions.
lower ::
  Parser Char
lower =
  satisfy isLower

-- Exercise 10.7
-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isUpper@ functions.
upper ::
  Parser Char
upper =
  satisfy isUpper

-- Exercise 10.8
-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isAlpha@ functions.
alpha ::
  Parser Char
alpha =
  satisfy isAlpha

-- Exercise 11
-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @bindParser@ and @value@.
-- /Tip:/ Optionally use @Prelude.foldr@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True
sequenceParser ::
  List (Parser a)
  -> Parser (List a)
sequenceParser Nil =
  valueParser Nil
sequenceParser (h:.t) =
  fbindParser h (\a ->
  fbindParser (sequenceParser t) (\as ->
  valueParser (a :. as)))

-- Exercise 12
-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @Prelude.replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany ::
  Int
  -> Parser a
  -> Parser (List a)
thisMany n p =
  sequenceParser (replicate n p)

-- Exercise 13
-- | Write a parser for Person.age.
--
-- /Age: positive integer/
--
-- /Tip:/ Equivalent to @natural@.
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True
ageParser ::
  Parser Int
ageParser =
  natural

-- Exercise 14
-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter/
--
-- /Tip:/ Use @bindParser@, @value@, @upper@, @list@ and @lower@.
--
-- λ> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- λ> isErrorResult (parse firstNameParser "abc")
-- True
firstNameParser ::
  Parser Str
firstNameParser =
  fbindParser upper (\c ->
  fbindParser (list lower) (\cs ->
  valueParser (c :. cs)))

-- Exercise 15
-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @bindParser@, @value@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
surnameParser ::
  Parser Str
surnameParser =
  fbindParser upper (\c ->
  fbindParser (thisMany 5 lower) (\cs ->
  fbindParser (list lower) (\t ->
  valueParser (c :. cs ++ t))))

-- Exercise 16
-- | Write a parser for Person.gender.
--
-- /Gender: character that must be @'m'@ or @'f'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse genderParser "mabc"
-- Result >abc< 'm'
--
-- >>> parse genderParser "fabc"
-- Result >abc< 'f'
--
-- >>> isErrorResult (parse genderParser "abc")
-- True
genderParser ::
  Parser Char
genderParser =
  is 'm' ||| is 'f'

-- Exercise 17
-- | Write part of a parser for Person.phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
--
-- /Phone: string of digits, dots or hyphens .../
--
-- /Tip:/ Use @list@, @digit@, @(|||)@ and @is@.
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> parse phoneBodyParser "a123-456"
-- Result >a123-456< ""
phoneBodyParser ::
  Parser Str
phoneBodyParser =
  list (digit ||| is '.' ||| is '-')

-- Exercise 18
-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @bindParser@, @value@, @digit@, @phoneBodyParser@ and @is@.
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
phoneParser ::
  Parser Str
phoneParser =
  fbindParser digit (\d ->
  fbindParser phoneBodyParser (\z ->
  fbindParser (is '#') (\_ ->
  valueParser (d :. z))))

-- Exercise 19
-- | Write a parser for Person.
--
-- /Tip:/ Use @bindParser@,
--            @value@,
--            @(>>>)@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @genderParser@,
--            @phoneParser@.
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
personParser ::
  Parser Person
personParser =
  fbindParser ageParser (\a ->
  spaces1 >>>
  fbindParser firstNameParser (\f ->
  spaces1 >>>
  fbindParser surnameParser (\s ->
  spaces1 >>>
  fbindParser genderParser (\g ->
  spaces1 >>>
  fbindParser phoneParser (\p ->
  valueParser (Person a f s g p))))))

-- Exercise 20
-- Make sure all the tests pass!


-- Exercise 20.1
-- | Write a Functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Functor Parser where
  (<$>) f =
    bindParser (valueParser . f)

-- | Write a Apply instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Apply Parser where
  p <*> q =
    bindParser (\f -> bindParser (\a -> valueParser (f a)) q) p

-- Exercise 20.2
-- | Write an Applicative functor instance for a @Parser@.
instance Applicative Parser where
  pure =
    valueParser

-- Exercise 20.3
-- | Write a Bind instance for a @Parser@.
instance Bind Parser where
  (=<<) =
    error "todo"

instance Monad Parser where

instance P.Monad Parser where
  (>>=) =
    flip (=<<)
  return =
    pure
