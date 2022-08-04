{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.AlternativeTest (
  -- * Tests
    test_Alternative
  , optionalTest
  , listTest
  , parserTest
  , manyTest
  , someTest
  , aconcatTest

  -- * Runner
  , test
  ) where

import           Test.Framework  (TestTree, testCase, testGroup, test, (@?=), testProperty)

import           Course.Core
import           Course.Alternative
import           Course.Optional ( Optional(Full, Empty) )
import           Course.List ( List(Nil, (:.)), listh )
import           Course.Parser
              ( character,
                constantParser,
                digit,
                isErrorResult,
                parse,
                valueParser,
                ParseResult(Result, UnexpectedEof) )
import           Course.Applicative ( (*>) )

test_Alternative :: TestTree
test_Alternative =
  testGroup "Alternative" [
    optionalTest
  , listTest
  , parserTest
  , manyTest
  , someTest
  , aconcatTest
  ]


optionalTest :: TestTree
optionalTest =
  testGroup "Optional" [
    testCase "left identity" $
      zero <|> Full 4 @?= Full 4
  , testCase "right identity" $
      Full 3 <|> zero @?= Full 3
  , testCase "all full" $
      Full 3 <|> Full 4 @?= Full 3
  ]

listTest :: TestTree
listTest =
  testGroup "List" [
    testCase "left identity" $
      zero <|> 6 :. 7 :. 8 :. Nil @?= 6 :. 7 :. 8 :. Nil
  , testCase "right identity" $
      3 :. 4 :. 5 :. Nil <|> zero @?= 3 :. 4 :. 5 :. Nil
  , testCase "all not Nil" $
      3 :. 4 :. 5 :. Nil <|> 6 :. 7 :. 8 :. Nil @?=
        3 :. 4 :. 5 :. 6 :. 7 :. 8 :. Nil
  ]

parserTest :: TestTree
parserTest =
  testGroup "Parser" [
    testCase "character or valueParser empty input" $
      parse (character <|> valueParser 'v') (listh "") @?= Result Nil 'v'
  , testCase "zero or valueParser empty input" $
      parse (zero <|> valueParser 'v') (listh "") @?= Result Nil 'v'
  , testCase "character or valueParser abc input" $
      parse (character <|> valueParser 'v') (listh "abc") @?= Result (listh "bc") 'a'
  , testCase "unexpectedEof or valueParser abc input" $
      parse (zero <|> valueParser 'v') (listh "abc") @?=
        Result (listh "abc") 'v'
  ]

manyTest :: TestTree
manyTest =
  testGroup "Many" [
    testCase "many character empty input" $
      parse (many character) (listh "") @?= Result Nil Nil
  , testCase "many digit 123abc input" $
      parse (many digit) (listh "123abc") @?= Result (listh "abc") (listh "123")
  , testCase "many digit abc input" $
      parse (many digit) (listh "abc") @?= Result (listh "abc") Nil
  , testCase "many character abc input" $
      parse (many character) (listh "abc") @?= Result Nil (listh "abc")
  , testCase "many (character to valueParser) abc input" $
      parse (many (character *> valueParser 'v')) (listh "abc") @?= Result Nil (listh "vvv")
  , testCase "many (character to valueParser) empty input" $
      parse (many (character *> valueParser 'v')) (listh "") @?= Result Nil Nil
  ]

someTest :: TestTree
someTest =
  testGroup "Some" [
    testCase "some character abc input" $
      parse (some character) (listh "abc") @?= Result Nil (listh "abc")
  , testCase "some (character to valueParser) abc input" $
      parse (some (character *> valueParser 'v')) (listh "abc") @?= Result Nil (listh "vvv")
  , testCase "some (character to valueParser) empty input" $
      isErrorResult (parse (some (character *> valueParser 'v')) (listh "")) @?= True
  ]

aconcatTest :: TestTree
aconcatTest =
  testGroup "Aconcat" [
    testCase "empty list" $
      aconcat (Nil :: List (List Int)) @?= Nil
  , testCase "several lists" $
      aconcat ((3 :. 4 :. Nil) :. Nil :. (5 :. 6 :. Nil) :. Nil) @?= 3 :. 4 :. 5 :. 6 :. Nil
  , testCase "several Optionals" $
      aconcat (Empty :. Empty :. Full 7 :. Empty :. Full 8 :. Empty :. Nil) @?= Full 7
  ]
