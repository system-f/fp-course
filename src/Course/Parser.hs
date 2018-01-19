{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Parser where

import Course.Core
import Course.Person
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional
import Data.Char

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Chars

data ParseResult a =
    UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString Chars
  | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character: ", show [c]]
  show (UnexpectedString s) =
    stringconcat ["Unexpected string: ", show s]
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]
  
instance Functor ParseResult where
  _ <$> UnexpectedEof =
    UnexpectedEof
  _ <$> ExpectedEof i =
    ExpectedEof i
  _ <$> UnexpectedChar c =
    UnexpectedChar c
  _ <$> UnexpectedString s =
    UnexpectedString s
  f <$> Result i a =
    Result i (f a)

-- Function to determine is a parse result is an error.
isErrorResult ::
  ParseResult a
  -> Bool
isErrorResult (Result _ _) =
  False
isErrorResult UnexpectedEof =
  True
isErrorResult (ExpectedEof _) =
  True
isErrorResult (UnexpectedChar _) =
  True
isErrorResult (UnexpectedString _) =
  True

-- | Runs the given function on a successful parse result. Otherwise return the same failing parse result.
onResult ::
  ParseResult a
  -> (Input -> a -> ParseResult b)
  -> ParseResult b
onResult UnexpectedEof _ = 
  UnexpectedEof
onResult (ExpectedEof i) _ = 
  ExpectedEof i
onResult (UnexpectedChar c) _ = 
  UnexpectedChar c
onResult (UnexpectedString s)  _ = 
  UnexpectedString s
onResult (Result i a) k = 
  k i a

data Parser a = P (Input -> ParseResult a)

parse ::
  Parser a -> Input -> ParseResult a
parse (P p) =
  p

-- | Produces a parser that always fails with @UnexpectedChar@ using the given character.
unexpectedCharParser ::
  Char
  -> Parser a
unexpectedCharParser c =
  P (\_ -> UnexpectedChar c)

--- | Return a parser that always returns the given parse result.
---
--- >>> isErrorResult (parse (constantParser UnexpectedEof) "abc")
--- True
constantParser ::
  ParseResult a
  -> Parser a
constantParser =
  P . const

-- | A parser that produces zero or a positive integer.
natural ::
  Parser Int
natural =
  bindParser (\k -> case read k of Empty  -> constantParser (UnexpectedString k)
                                   Full h -> valueParser h) (list1 digit)

-- | Return a parser that always succeeds with the given value and consumes no input.
--
-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3
valueParser ::
  a
  -> Parser a
valueParser =
  \a -> P (\i -> Result i a)

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
  P (\i -> case i of
             Nil -> UnexpectedEof
             h:.t -> Result t h)

  -- P (\i -> if i is empty then fail else succeed with the head with the rest being the tail (of i))

-- | Return a parser that maps any succeeding result with the given function.
--
-- >>> parse (mapParser succ character) "amz"
-- Result >mz< 'b'
--
-- >>> parse (mapParser (+10) (valueParser 7)) ""
-- Result >< 17
mapParser ::
  (a -> b)
  -> Parser a
  -> Parser b
mapParser =
  -- f :: a -> b
  -- parse p :: Input -> ParseResult a
  ----
  -- ? :: ParseResult b

  -- (<$>) :: (a -> b) -> ParseResult a -> ParseResult b
  \f p ->
    P (\i -> f <$> parse p i)

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
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
-- onResult :: ParseResult a -> (Input -> a -> ParseResult b) -> ParseResult b
bindParser =
  \f p ->
    P (\i -> onResult (parse p i)
                      (\r a -> parse (f a) r))

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @bindParser@ or @>>=@.
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
(>>>) pa pb =
  (flip const <$> pa) <*> pb

-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (constantParser UnexpectedEof ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (constantParser UnexpectedEof ||| valueParser 'v') "abc"
-- Result >abc< 'v'
(|||) ::
  Parser a
  -> Parser a
  -> Parser a
(|||) =
  \p1 p2 ->
    P (\i -> let r = parse p1 i
             in  bool r (parse p2 i) (isErrorResult r))

infixl 3 |||

-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @list1@, @valueParser@ and @(|||)@.
--
-- >>> parse (list character) ""
-- Result >< ""
--
-- >>> parse (list digit) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list character) "abc"
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




list freds =
  list1 freds ||| pure Nil


-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
--
-- /Tip:/ Use @bindParser@, @list@ and @valueParser@.
--
-- >>> parse (list1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (list1 (character *> valueParser 'v')) "")
-- True
list1 ::
  Parser a
  -> Parser (List a)
list1 parsefred =
  do 
      fred <- parsefred
      otherfreds <- list parsefred
      pure ((:.) fred otherfreds)

{-

* insert the word `do`
* turn `>>=` into `<-`
* delete `->`
* delete `\`
* swap each side of `<-`
-}

{-
  parsefred >>= \fred ->
  list parsefred >>= \otherfreds ->
  pure ((:.) fred otherfreds)
-}

{-
(1 or many) parsefred =
  parsefred >>= \fred)
  (0 or many) parsefred >>= \otherfreds)
  always ((:.) fred otherfreds)
-}

-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @bindParser@, @unexpectedCharParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy ::
  (Char -> Bool)
  -> Parser Char
satisfy =
  \predicate -> character >>= 
    lift3 bool unexpectedCharParser pure predicate
-- \c -> bool (unexpectedCharParser c) (pure c) (predicate c)



--  if predicate c then pure c else unexpectedCharParser c

-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
is ::
  Char -> Parser Char
is =
-- \c -> satisfy (\d -> c == d)
-- \c -> satisfy (\d -> (==) c d)
-- \c -> satisfy ((==) c)
-- \c -> satisfy ((==) c)
  satisfy . (==)

-- \x -> f (g x)
-- f . g


-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isDigit@ functions.
digit ::
  Parser Char
digit =
  satisfy isDigit

--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isSpace@ functions.
space ::
  Parser Char
space =
  satisfy isSpace

-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @list1@ and @space@ functions.
spaces1 ::
  Parser Chars
spaces1 =
  list1 space

-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isLower@ functions.
lower ::
  Parser Char
lower =
  satisfy isLower

-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isUpper@ functions.
upper ::
  Parser Char
upper =
  satisfy isUpper

-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char#isAlpha@ functions.
alpha ::
  Parser Char
alpha =
  satisfy isAlpha

-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @bindParser@ and @valueParser@.
-- /Tip:/ Optionally use @List#foldRight@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True
sequenceParser ::
  List (Parser a)
  -> Parser (List a)
sequenceParser Nil = pure Nil
sequenceParser (h:.t) =
  lift2 (:.) h (sequenceParser t)
  {-
  do  a <- h
      b <- sequenceParser t
      pure (a :. b)
-}

  {-
  h >>= \a ->
  sequenceParser t >>= \b ->
  pure (a :. b)
-}
-- h :: Parser a
                              -- t :: List (Parser a)
-- sequenceParser t :: Parser (List a)
--
-- ? :: Parser (List a)


-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @List.replicate@.
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
thisMany =
-- replicate :: (Ord n, Num n) => n -> a -> List a
-- \n -> \p -> sequenceParser (replicate n p)
-- \n -> sequenceParser . replicate n
-- \n -> (.) sequenceParser (replicate n)
-- \n -> (.) sequenceParser (replicate n)
-- (.) sequenceParser . replicate
  (sequenceParser .) . replicate

-- \x -> f (g x)
-- f . g


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

-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters/
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @list@ and @lower@.
--
-- >>> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- >>> isErrorResult (parse firstNameParser "abc")
-- True
firstNameParser ::
  Parser Chars
firstNameParser =
  upper >>= \c ->
  list lower >>= \d ->
  pure (c :. d)

-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> parse surnameParser "Abcdefghijklmnopqrstuvwxyz"
-- Result >< "Abcdefghijklmnopqrstuvwxyz"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
surnameParser ::
  Parser Chars
surnameParser =
  do  x <- upper
      y <- thisMany 5 lower
      z <- list lower
      pure (x :. y ++ z)

  {-
  upper >>= \damo ->
  thisMany 5 lower >>= \dazza ->
  list lower >>= \bazza ->
  pure (damo :. dazza ++ bazza)
-}

-- | Write a parser for Person.smoker.
--
-- /Smoker: character that must be @'y'@ or @'n'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse smokerParser "yabc"
-- Result >abc< 'y'
--
-- >>> parse smokerParser "nabc"
-- Result >abc< 'n'
--
-- >>> isErrorResult (parse smokerParser "abc")
-- True
smokerParser ::
  Parser Char
smokerParser =
  is 'y' ||| is 'n'

-- | Write part of a parser for Person#phoneBody.
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
  Parser Chars
phoneBodyParser =
  list (digit ||| is '.' ||| is '-')

-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @digit@, @phoneBodyParser@ and @is@.
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
  Parser Chars
phoneParser =
  digit >>= \d ->
  phoneBodyParser >>= \b ->
  is '#' >>= \_ ->
  pure (d :. b)

-- | Write a parser for Person.
--
-- /Tip:/ Use @bindParser@,
--            @valueParser@,
--            @(>>>)@,
--            @spaces1@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @smokerParser@,
--            @phoneParser@.
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
personParser ::
  Parser Person
personParser =
  Person <$>
    ageParser <*>
    spaces1 *>
    firstNameParser <*>
    spaces1 *>
    surnameParser <*>
    spaces1 *>
    smokerParser <*>
    spaces1 *>
    phoneParser
    
  {-
  do  a <- ageParser
      spaces1
      f <- firstNameParser
      spaces1
      s <- surnameParser
      spaces1
      k <- smokerParser
      spaces1
      p <- phoneParser
      pure (Person a f s k p)
  -}

  {-
  ageParser >>= \a ->
  spaces1 >>= \_ ->
  firstNameParser >>= \f ->
  spaces1 >>= \_ ->
  surnameParser >>= \s ->
  spaces1 >>= \_ ->
  smokerParser >>= \k ->
  spaces1 >>= \_ ->
  phoneParser >>= \p ->
  pure (Person a f s k p)
  -}

  
-- Make sure all the tests pass!


-- | Write a Functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Functor Parser where
  (<$>) ::
    (a -> b)
    -> Parser a
    -> Parser b
  (<$>) =
    mapParser

-- | Write an Applicative functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Applicative Parser where
  pure ::
    a
    -> Parser a
  pure =
    valueParser
  (<*>) ::
    Parser (a -> b)
    -> Parser a
    -> Parser b
  (<*>) =
-- bindParser :: (x -> Parser y) -> Parser x -> Parser y
-- eff :: a -> b
-- gee :: a
----
-- ? :: Parser b
    \pab pa ->
      bindParser (\eff -> mapParser eff pa) pab
    {-
      pab >>= \eff ->
      pa  >>= \gee ->
      valueParser (eff gee)
      -}
    {-
      bindParser (\eff -> 
        bindParser (\gee -> 
          valueParser (eff gee)) pa) pab
    -}

-- | Write a Monad instance for a @Parser@.
instance Monad Parser where
  (=<<) ::
    (a -> Parser b)
    -> Parser a
    -> Parser b
  (=<<) =
    bindParser
