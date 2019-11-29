{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.FunctorTest (
  -- * Tests
    test_Functor
  , idTest
  , listTest
  , optionalTest
  , functionTest
  , anonMapTest
  , voidTest

  -- * Runner
  , test
  ) where

import           Test.Framework    (TestTree, testCase, testGroup,
                                    testProperty, test, (@?=))

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (ExactlyOne))
import           Course.Functor    (void, (<$), (<$>))
import           Course.List       (List ((:.), Nil))
import           Course.Optional   (Optional (Empty, Full))

test_Functor :: TestTree
test_Functor =
  testGroup "Functor" [
    idTest
  , listTest
  , optionalTest
  , functionTest
  , anonMapTest
  , voidTest
  ]

idTest :: TestTree
idTest =
  testCase "ExactlyOne" $ (+1) <$> ExactlyOne 2 @?= ExactlyOne 3

listTest :: TestTree
listTest =
  testGroup "List" [
    testCase "empty list" $
      (+1) <$> Nil @?= Nil
  , testCase "increment" $
      (+1) <$> (1 :. 2 :. 3 :. Nil) @?= (2 :. 3 :. 4 :. Nil)
  ]

optionalTest :: TestTree
optionalTest =
  testGroup "Optional" [
    testCase "Empty" $ (+1) <$> Empty @?= Empty
  , testCase "Full"  $ (+1) <$> Full 2 @?= Full 3
  ]

functionTest :: TestTree
functionTest =
  testCase "(->)" $ ((+1) <$> (*2)) 8 @?= 17


anonMapTest :: TestTree
anonMapTest =
  testGroup "(<$)" [
    testCase "Map 7" $ 7 <$ (1 :. 2 :. 3 :. Nil) @?= (7 :. 7 :. 7 :. Nil)
  , testProperty "Always maps a constant value over List" $ \x a b c ->
      (x :: Integer) <$ ((a :. b :. c :. Nil) :: List Integer) == (x :. x :. x :. Nil)
  , testProperty "Always maps a constant value over Full (Optional)" $ \x q ->
      x <$ Full (q :: Integer) == Full (x :: Integer)
  ]

voidTest :: TestTree
voidTest =
  testGroup "void" [
    testCase "List"  $ void (1 :. 2 :. 3 :. Nil) @?= () :. () :. () :. Nil
  , testCase "Full"  $ void (Full 7) @?= Full ()
  , testCase "Empty" $ void Empty @?= Empty
  , testCase "(->)"  $ void (+10) 5 @?= ()
  ]
