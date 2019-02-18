{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ParserTest where

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=), assertBool)

import           Course.Applicative    (pure, (<*>), (*>))
import           Course.Core
import           Course.List           (List (..))
import           Course.Monad          ((=<<))
import           Course.Optional       (Optional (..))
import           Course.Parser
import           Course.Person         (Person(..))

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
      constantParserTest
    , characterTest
    , valueParserTest
    , alternativeParserTest
    , parserMonadInstanceTest
    , parserApplicativeInstanceTest
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
    , testCase "errors on empty string" $
        assertBool "parsing empty string is an error" (isErrorResult (parse character ""))
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
      testCase "takes the second result when the first fails" $ do
        parse (character ||| valueParser 'v') "" @?= Result "" 'v'
        parse (constantParser UnexpectedEof ||| valueParser 'v') "" @?= Result "" 'v'
        parse (constantParser UnexpectedEof ||| valueParser 'v') "abc" @?= Result "abc" 'v'
    , testCase "takes first parse result when it succeeds" $
        parse (character ||| valueParser 'v') "abc" @?= Result "bc" 'a'
  ]

parserMonadInstanceTest :: TestTree
parserMonadInstanceTest =
  testGroup "parserMonadInstanceTest" [
      testCase "if parser fails with error, the returned parser fails with error" $ do
        assertBool
          "bind propgates error"
          (isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) ""))
        assertBool
          "bind propogates error"
          (isErrorResult (parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "x"))
    , testCase "if parser succeeds, value passed to bind function and input propogated" $ do
        parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "abc" @?= Result "bc" 'v'
        parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "a" @?= Result "" 'v'
        parse ((\c -> if c == 'x' then character else valueParser 'v') =<< character) "xabc" @?= Result "bc" 'a'
  ]

parserApplicativeInstanceTest :: TestTree
parserApplicativeInstanceTest =
  testGroup "parserApplicativeInstanceTest" [
      testCase "pure" $ do
        parse (pure 'a' :: Parser Char) "xyz" @?= Result "xyz" 'a'
        parse (pure (Full 5) :: Parser (Optional Int)) "xyz" @?= Result "xyz" (Full 5)
    , testCase "<*>" $ do
        parse (valueParser toUpper <*> valueParser 'a') "xyz" @?= Result "xyz" 'A'
        parse (valueParser show <*> valueParser 599) "xyz" @?= Result "xyz" "599"
  ]

satisfyTest :: TestTree
satisfyTest =
  testGroup "satisfyTest" [
      testCase "when character satisfies predicate" $
        parse (satisfy isUpper) "Abc" @?= Result "bc" 'A'
    , testCase "when character does not satisfy predicate" $
        assertBool "is error when preidcate not satisfied" (isErrorResult (parse (satisfy isUpper) "abc"))
  ]

digitTest :: TestTree
digitTest =
  testGroup "digitTest" [
      testCase "fails when input empty" $
        assertBool "is error when input empty" (isErrorResult $ parse digit "")
    , testCase "fails when character not digit" $
        assertBool "is error when character not digit" (isErrorResult $ parse digit "ABC")
    , testCase "succeeds when character is a digit  " $
        parse digit "1BC" @?= Result "BC" '1'
  ]

spaceTest :: TestTree
spaceTest =
  testGroup "spaceTest" [
      testCase "fails when input empty" $
        assertBool "is error when input empty" (isErrorResult $ parse space "")
    , testCase "fails when character not space" $
        assertBool "is error when character not space" (isErrorResult $ parse space "ABC")
    , testCase "succeeds when character is a space" $
        parse space " abc" @?= Result "abc" ' '
  ]

listTest :: TestTree
listTest =
  testGroup "listTest" [
      testCase "succeeds on empty input" $
        parse (list character) "" @?= Result "" ""
    , testCase "parses for as long as characters match" $ do
        parse (list digit) "123abc" @?= Result "abc" "123"
        parse (list digit) "abc" @?= Result "abc" ""
        parse (list character) "abc" @?= Result "" "abc"
        parse (list (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
        parse (list (character *> valueParser 'v')) "" @?= Result "" ""
  ]

list1Test :: TestTree
list1Test =
  testGroup "list1Test" [
      testCase "succeeds when at least one character matches" $ do
        parse (list1 (character)) "abc" @?= Result "" "abc"
        parse (list1 (character *> valueParser 'v')) "abc" @?= Result "" "vvv"
    , testCase "fails when no characters match" $
        assertBool "no matching chars fails" (isErrorResult (parse (list1 (character *> valueParser 'v')) ""))
  ]

spaces1Test :: TestTree
spaces1Test =
  testGroup "spaces1Test" [
      testCase "fails on empty string" $
        assertBool "fails on empty string" (isErrorResult (parse spaces1 ""))
    , testCase "consumes single space" $
        parse spaces1 " " @?= Result "" " "
    , testCase "consumes multiple spaces" $
        parse spaces1 "    abc" @?= Result "abc" "    "
  ]

lowerTest :: TestTree
lowerTest =
  testGroup "lowerTest" [
      testCase "fails on empty string" $
        assertBool "fails on empty string" (isErrorResult (parse lower ""))
    ,  testCase "fails if character is not lowercase" $
        assertBool "fails if character is not lowercase" (isErrorResult (parse lower "Abc"))
    , testCase "produces lowercase character" $
        parse lower "aBC" @?= Result "BC" 'a'
  ]

upperTest :: TestTree
upperTest =
  testGroup "upperTest" [
      testCase "fails on empty string" $
        assertBool "fails on empty string" (isErrorResult (parse upper ""))
    ,  testCase "fails if character is not uppercase" $
        assertBool "fails if character is not uppercase" (isErrorResult (parse upper "aBC"))
    , testCase "produces uppercase character" $
        parse upper "Abc" @?= Result "bc" 'A'
  ]

alphaTest :: TestTree
alphaTest =
  testGroup "alphaTest" [
      testCase "fails on empty string" $
        assertBool "fails on empty string" (isErrorResult (parse alpha ""))
    ,  testCase "fails if character is not alpha" $
        assertBool "fails if character is not alpha" (isErrorResult (parse alpha "5BC"))
    , testCase "produces alpha character" $
        parse upper "A45" @?= Result "45" 'A'
  ]

sequenceParserTest :: TestTree
sequenceParserTest =
  testGroup "sequenceParserTest" [
      testCase "fails on first failing parser" $
        assertBool "fails on first failing parser" $
          isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
    , testCase "sequences list of successful parsers" $
        parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef" @?= Result "def" "axC"
  ]

thisManyTest :: TestTree
thisManyTest =
  testGroup "thisManyTest" [
      testCase "fails when not enough matches" $
        assertBool "fails when not enough matches" $
          isErrorResult (parse (thisMany 4 upper) "ABcDef")
    , testCase "produces n values when matched" $
        parse (thisMany 4 upper) "ABCDef" @?= Result "ef" "ABCD"
  ]

ageParserTest :: TestTree
ageParserTest =
  testGroup "ageParserTest (done for you)" [
      testCase "fails on invalid age" $ do
        assertBool "fails on invalid age" $
          isErrorResult (parse ageParser "abc")
        assertBool "fails on invalid age" $
          isErrorResult (parse ageParser "-120")
    , testCase "parses valid age" $
        parse ageParser "120" @?= Result "" 120
  ]

firstNameParserTest :: TestTree
firstNameParserTest =
  testGroup "firstNameParserTest" [
      testCase "fails on invalid first name" $
        assertBool "fails on invalid first name" $
          isErrorResult (parse firstNameParser "abc")
    , testCase "parses valid first name" $
        parse firstNameParser "Abc" @?= Result "" "Abc"
  ]

surnameParserTest :: TestTree
surnameParserTest =
  testGroup "surnameParserTest" [
      testCase "fails on invalid surname" $ do
        assertBool "fails on invalid surname" $
          isErrorResult (parse surnameParser "Abc")
        assertBool "fails on invalid surname" $
          isErrorResult (parse surnameParser "abc")
    , testCase "parses valid surname" $ do
        parse surnameParser "Abcdef" @?= Result "" "Abcdef"
        parse surnameParser "Abcdefghijklmnopqrstuvwxyz" @?= Result "" "Abcdefghijklmnopqrstuvwxyz"
  ]

smokerParserTest :: TestTree
smokerParserTest =
  testGroup "smokerParserTest" [
      testCase "fails on non y/n value" $
        assertBool "fails on non y/n value" $
          isErrorResult (parse smokerParser "abc")
    , testCase "parses valid smoker value" $ do
        parse smokerParser "yabc" @?= Result "abc" True
        parse smokerParser "nabc" @?= Result "abc" False
  ]

phoneBodyParserTest :: TestTree
phoneBodyParserTest =
  testGroup "phoneBodyParserTest" [
      testCase "produces empty list when no characters match" $
        parse phoneBodyParser "a123-456" @?= Result "a123-456" ""
    , testCase "parses valid phone body value" $ do
        parse phoneBodyParser "123-456" @?= Result "" "123-456"
        parse phoneBodyParser "123-a456" @?= Result "a456" "123-"
  ]

phoneParserTest :: TestTree
phoneParserTest =
  testGroup "phoneParserTest" [
      testCase "fails on invalid phone values" $ do
        assertBool "fails on invalid phone values" $
          isErrorResult (parse phoneParser "123-456")
        assertBool "fails on invalid phone values" $
          isErrorResult (parse phoneParser "a123-456")
    , testCase "produces valid phone numbers" $ do
        parse phoneParser "123-456#" @?= Result "" "123-456"
        parse phoneParser "123-456#abc" @?= Result "abc" "123-456"
  ]

personParserTest :: TestTree
personParserTest =
  testGroup "personParserTest" [
      testCase "fails in invalid inputs" $ do
        assertBool "fails on empty string" $
          isErrorResult (parse personParser "")
        assertBool "fails on invalid age" $
          isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
        assertBool "fails on first name that doesn't start with capital" $
          isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
        assertBool "fails on surname that is too short" $
          isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
        assertBool "fails on surname that doesn't start with a capital letter" $
          isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
        assertBool "fails on invalid smoker value" $
          isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
        assertBool "fails on invalid phone number" $
          isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
        assertBool "fails on invalid phone number" $
          isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
        assertBool "fails on invalid phone number" $
          isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
    , testCase "produces person for valid input" $
        parse personParser "123 Fred Clarkson y 123-456.789#" @?=
          Result "" (Person 123 "Fred" "Clarkson" True "123-456.789")

  ]
