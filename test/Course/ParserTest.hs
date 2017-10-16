{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import           Course.Core
import           Course.Parser    ()

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
    constantParserTest
  , valueParserTest
  , characterTest
  , mapParserTest
  , bindParserTest
  , ignoreFirstTest
  , alternationTest
  , listTest
  , list1Test
  , satisfyTest
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
  ]

valueParserTest :: TestTree
valueParserTest =
  testGroup "valueParserTest" [
  ]

characterTest :: TestTree
characterTest =
  testGroup "characterTest" [
  ]

mapParserTest :: TestTree
mapParserTest =
  testGroup "mapParserTest" [
  ]

bindParserTest :: TestTree
bindParserTest =
  testGroup "bindParserTest" [
  ]

ignoreFirstTest :: TestTree
ignoreFirstTest =
  testGroup "ignoreFirstTest" [
  ]

alternationTest :: TestTree
alternationTest =
  testGroup "alternationTest" [
  ]

listTest :: TestTree
listTest =
  testGroup "listTest" [
  ]

list1Test :: TestTree
list1Test =
  testGroup "list1Test" [
  ]

satisfyTest :: TestTree
satisfyTest =
  testGroup "satisfyTest" [
  ]

sequenceParserTest :: TestTree
sequenceParserTest =
  testGroup "sequenceParserTest" [
  ]

thisManyTest :: TestTree
thisManyTest =
  testGroup "thisManyTest" [
  ]

ageParserTest :: TestTree
ageParserTest =
  testGroup "ageParserTest" [
  ]

firstNameParserTest :: TestTree
firstNameParserTest =
  testGroup "firstNameParserTest" [
  ]

surnameParserTest :: TestTree
surnameParserTest =
  testGroup "surnameParserTest" [
  ]

smokerParserTest :: TestTree
smokerParserTest =
  testGroup "smokerParserTest" [
  ]

phoneBodyParserTest :: TestTree
phoneBodyParserTest =
  testGroup "phoneBodyParserTest" [
  ]

phoneParserTest :: TestTree
phoneParserTest =
  testGroup "phoneParserTest" [
  ]

personParserTest :: TestTree
personParserTest =
  testGroup "personParserTest" [
  ]
