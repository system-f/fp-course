{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ParserTest (
  -- * Tests
    test_Parser
  , constantParserTest
  , characterTest
  , functorTest
  , valueParserTest
  , alternativeParserTest
  , monadTest
  , applicativeTest
  , satisfyTest
  , digitTest
  , spaceTest
  , listTest
  , list1Test
  , spaces1Test
  , lowerTest
  , upperTest
  , alphaTest
  , sequenceParserTest
  , thisManyTest
  , ageParserTest
  , firstNameParserTest
  , surnameParserTest
  , smokerParserTest
  , phoneBodyParserTest
  , phoneParserTest
  , personParserTest

  -- * Runner
  , test
  ) where

import           Test.Framework     (TestTree, assertBool, testCase,
                                     testGroup, test, (@?=))

import           Course.Applicative (pure, (*>), (<*>))
import           Course.Core
import           Course.Functor     ((<$>))
import           Course.List        (List ((:.), Nil))
import           Course.Monad       ((=<<))
import           Course.Optional    (Optional (Full))
import           Course.Parser
import           Course.Person      (Person (Person))

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
      constantParserTest
    , characterTest
    , functorTest
    , valueParserTest
    , alternativeParserTest
    , monadTest
    , applicativeTest
    , satisfyTest
    , digitTest
    , spaceTest
    , listTest
    , list1Test
    , spaces1Test
    , lowerTest
    , upperTest
    , alphaTest
    , sequenceParserTest
    , thisManyTest
    , ageParserTest
    , firstNameParserTest
    , surnameParserTest
    , smokerParserTest
    , phoneBodyParserTest
    , phoneParserTest
    , personParserTest
  ]

constantParserTest :: TestTree
constantParserTest =
  testGroup "constantParserTest" [
      testCase "can return error result" $
        parse (constantParser (UnexpectedEof :: ParseResult Int)) "abc" @?= (UnexpectedEof :: ParseResult Int)
    , testCase "can return ParseResult" $
        parse (constantParser (Result "xyz" 4)) "abc" @?= Result "xyz" 4
  ]

characterTest :: TestTree
characterTest =
  testGroup "characterTest" [
      testCase "parses single character from non-empty string" $
        parse character "abc" @?= Result "bc" 'a'
    , assertBool "parsing empty string is an error" $
        isErrorResult (parse character "")
  ]

functorTest :: TestTree
functorTest =
  testGroup "functorTest" [
    testCase "toUpper <$>" $
      parse (toUpper <$> character) "amz" @?= Result "mz" 'A'
  ]

valueParserTest :: TestTree
valueParserTest =
  testGroup "valueParserTest" [
      testCase "succeeds with given value" $
        parse (valueParser 3) "abc" @?= Result "abc" 3
  ]

alternativeParserTest :: TestTree
alternativeParserTest =
  testGroup "alternativeParserTest" [
      testCase "first fails, second succeeds with no input" $
        parse (character ||| valueParser 'v') "" @?= Result "" 'v'
    , testCase "first always fails, second succeeds with no input" $
        parse (constantParser UnexpectedEof ||| valueParser 'v') "" @?= Result "" 'v'
    , testCase "first always fails, second succeeds with input" $
        parse (constantParser UnexpectedEof ||| valueParser 'v') "abc" @?= Result "abc" 'v'
    , testCase "takes first parse result when it succeeds" $
        parse (character ||| valueParser 'v') "abc" @?= Result "bc" 'a'
  ]

monadTest :: TestTree
monadTest =
  testGroup "parserMonadInstanceTest" [
      assertBool "first parse fails" $
        isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "")
    , assertBool "second parse fails" $
        isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "x")
    , testCase "bind to valueParser" $
        parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "abc" @?= Result "bc" 'v'
    , testCase "bind to valueParser with no more input" $
        parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "a" @?= Result "" 'v'
    , testCase "bind to character parser with remaining input" $
        parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "xabc" @?= Result "bc" 'a'
  ]

applicativeTest :: TestTree
applicativeTest =
  testGroup "parserApplicativeInstanceTest" [
      testCase "pure" $
        parse (pure 'a' :: Parser Char) "xyz" @?= Result "xyz" 'a'
    , testCase "pure an Optional value" $
        parse (pure (Full 5) :: Parser (Optional Int)) "xyz" @?= Result "xyz" (Full 5)
    , testCase "pure toUpper <*>" $
        parse (pure toUpper <*> valueParser 'a') "xyz" @?= Result "xyz" 'A'
    , testCase "pure show <*>" $
        parse (pure show <*> valueParser 599) "xyz" @?= Result "xyz" "599"
    , testCase "append character <*>" $
        parse (((\a b -> a :. b :. Nil) <$> character) <*> character) "abxyz" @?= Result "xyz" "ab"
  ]

satisfyTest :: TestTree
satisfyTest =
  testGroup "satisfyTest" [
      testCase "when character satisfies predicate" $
        parse (satisfy isUpper) "Abc" @?= Result "bc" 'A'
    , assertBool "is error when preidcate not satisfied" $
        isErrorResult (parse (satisfy isUpper) "abc")
  ]

digitTest :: TestTree
digitTest =
  testGroup "digitTest" [
      assertBool "is error when input empty" $
        isErrorResult $ parse digit ""
    , assertBool "is error when character not digit" $
        isErrorResult $ parse digit "ABC"
    , testCase "succeeds when character is a digit  " $
        parse digit "1BC" @?= Result "BC" '1'
  ]

spaceTest :: TestTree
spaceTest =
  testGroup "spaceTest" [
      assertBool "fails when input empty" $
        isErrorResult $ parse space ""
    , assertBool "fails when character not space" $
        isErrorResult $ parse space "ABC"
    , testCase "succeeds when first character is a space" $
        parse space " abc" @?= Result "abc" ' '
  ]

listTest :: TestTree
listTest =
  testGroup "listTest" [
      testCase "succeeds on empty input" $
        parse (list character) "" @?= Result "" ""
    , testCase "parses for as long as characters match" $
        parse (list digit) "123abc" @?= Result "abc" "123"
    , testCase "parses empty value when no matching characters" $
        parse (list digit) "abc" @?= Result "abc" ""
    , testCase "parses entire input if matches" $
        parse (list character) "abc" @?= Result "" "abc"
    , testCase "parses for as long as characters match" $
        parse (list (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
    , testCase "succeeds on empty input with value parser" $
        parse (list (character *> valueParser 'v')) "" @?= Result "" ""
  ]

list1Test :: TestTree
list1Test =
  testGroup "list1Test" [
      testCase "succeeds when at least one character matches" $
        parse (list1 character) "abc" @?= Result "" "abc"
    , testCase "succeeds when at least one character matches" $
        parse (list1 (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
    , assertBool "no matching chars fails" $
        isErrorResult (parse (list1 (character *> valueParser 'v')) "")
  ]

spaces1Test :: TestTree
spaces1Test =
  testGroup "spaces1Test" [
      assertBool "fails on empty string" $
        isErrorResult (parse spaces1 "")
    , testCase "consumes single space" $
        parse spaces1 " " @?= Result "" " "
    , testCase "consumes multiple spaces" $
        parse spaces1 "    abc" @?= Result "abc" "    "
  ]

lowerTest :: TestTree
lowerTest =
  testGroup "lowerTest" [
      assertBool "fails on empty string" $
        isErrorResult (parse lower "")
    , assertBool "fails if character is not lowercase" $
        isErrorResult (parse lower "Abc")
    , testCase "produces lowercase character" $
        parse lower "aBC" @?= Result "BC" 'a'
  ]

upperTest :: TestTree
upperTest =
  testGroup "upperTest" [
      assertBool "fails on empty string" $
        isErrorResult (parse upper "")
    , assertBool "fails if character is not uppercase" $
        isErrorResult (parse upper "aBC")
    , testCase "produces uppercase character" $
        parse upper "Abc" @?= Result "bc" 'A'
  ]

alphaTest :: TestTree
alphaTest =
  testGroup "alphaTest" [
      assertBool "fails on empty string" $
        isErrorResult (parse alpha "")
    , assertBool "fails if character is not alpha" $
        isErrorResult (parse alpha "5BC")
    , testCase "produces alpha character" $
        parse upper "A45" @?= Result "45" 'A'
  ]

sequenceParserTest :: TestTree
sequenceParserTest =
  testGroup "sequenceParserTest" [
      assertBool "fails on first failing parser" $
        isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
    , testCase "sequences list of successful parsers" $
        parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef" @?= Result "def" "axC"
  ]

thisManyTest :: TestTree
thisManyTest =
  testGroup "thisManyTest" [
      assertBool "fails when not enough matches" $
        isErrorResult (parse (thisMany 4 upper) "ABcDef")
    , testCase "produces n values when matched" $
        parse (thisMany 4 upper) "ABCDef" @?= Result "ef" "ABCD"
  ]

ageParserTest :: TestTree
ageParserTest =
  testGroup "ageParserTest (done for you)" [
      assertBool "fails on invalid age (all letters)" $
        isErrorResult (parse ageParser "abc")
    , assertBool "fails on invalid age (leading '-')" $
        isErrorResult (parse ageParser "-120")
    , testCase "parses valid age" $
        parse ageParser "120" @?= Result "" 120
  ]

firstNameParserTest :: TestTree
firstNameParserTest =
  testGroup "firstNameParserTest" [
      assertBool "fails on first name that doesn't start with a capital" $
        isErrorResult (parse firstNameParser "abc")
    , testCase "parses valid first name" $
        parse firstNameParser "Abc" @?= Result "" "Abc"
  ]

surnameParserTest :: TestTree
surnameParserTest =
  testGroup "surnameParserTest" [
      assertBool "fails on short surname" $
        isErrorResult (parse surnameParser "Abc")
    , assertBool "fails on short surname starting with a lower case letter" $
        isErrorResult (parse surnameParser "abc")
    , testCase "parses shortest valid surname" $
        parse surnameParser "Abcdef" @?= Result "" "Abcdef"
    , testCase "parses long surname" $
        parse surnameParser "Abcdefghijklmnopqrstuvwxyz" @?= Result "" "Abcdefghijklmnopqrstuvwxyz"
  ]

smokerParserTest :: TestTree
smokerParserTest =
  testGroup "smokerParserTest" [
      assertBool "fails on non y/n value" $
        isErrorResult (parse smokerParser "abc")
    , testCase "parses y, leaving remaining input" $
        parse smokerParser "yabc" @?= Result "abc" True
    , testCase "parses n, leaving remaining input" $
        parse smokerParser "nabc" @?= Result "abc" False
  ]

phoneBodyParserTest :: TestTree
phoneBodyParserTest =
  testGroup "phoneBodyParserTest" [
      testCase "produces empty list when no characters match" $
        parse phoneBodyParser "a123-456" @?= Result "a123-456" ""
    , testCase "parses valid phone body value" $
        parse phoneBodyParser "123-456" @?= Result "" "123-456"
    , testCase "parses up to first letter" $
        parse phoneBodyParser "123-a456" @?= Result "a456" "123-"
  ]

phoneParserTest :: TestTree
phoneParserTest =
  testGroup "phoneParserTest" [
      assertBool "fails without trailing '#'" $
        isErrorResult (parse phoneParser "123-456")
    , assertBool "fails when input starts with a letter" $
        isErrorResult (parse phoneParser "a123-456")
    , testCase "produces valid phone number" $
        parse phoneParser "123-456#" @?= Result "" "123-456"
    , testCase "produces a valid phone number with remaining input" $
        parse phoneParser "123-456#abc" @?= Result "abc" "123-456"
  ]

personParserTest :: TestTree
personParserTest =
  testGroup "personParserTest" [
      assertBool "fails on empty string" $
        isErrorResult (parse personParser "")
    , assertBool "fails on invalid age" $
        isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
    , assertBool "fails on first name that doesn't start with capital" $
        isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
    , assertBool "fails on surname that is too short" $
        isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
    , assertBool "fails on surname that doesn't start with a capital letter" $
        isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
    , assertBool "fails on invalid smoker value 'x'" $
        isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
    , assertBool "fails on phone number containing an 'x'" $
        isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
    , assertBool "fails on phone number starting with '-'" $
        isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
    , assertBool "fails on phone number without a trailing '#'" $
        isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
    , testCase "produces person for valid input" $
        parse personParser "123 Fred Clarkson y 123-456.789#" @?=
          Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
    , testCase "produces person for valid input and keeps remaining input" $
        parse personParser "123 Fred Clarkson y 123-456.789# rest" @?=
          Result " rest" (Person 123 "Fred" "Clarkson" True "123-456.789")
    , testCase "produces person for valid input containing extra whitespace" $
        parse personParser "123  Fred   Clarkson    y     123-456.789#" @?=
          Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")
  ]
