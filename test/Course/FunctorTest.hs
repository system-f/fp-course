{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FunctorTest (
  -- * Tests
    test_Functor
  , idTest
  , listTest
  , optionalTest
  , functionTest
  , anonMapTest
  , voidTest

  -- * Course test runner
  , courseTest
  ) where

import           Course.Gens       (genInteger)
import           Test.Course.Mini  (courseTest)
import           Test.Mini         (MiniTestTree, Testable (Fn), fn, testCase,
                                    testGroup, testProperty, (@?=))

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (ExactlyOne))
import           Course.Functor    (void, (<$), (<$>))
import           Course.List       (List ((:.), Nil))
import           Course.Optional   (Optional (Empty, Full))

test_Functor :: MiniTestTree
test_Functor =
  testGroup "Functor" [
    idTest
  , listTest
  , optionalTest
  , functionTest
  , anonMapTest
  , voidTest
  ]

idTest :: MiniTestTree
idTest =
  testCase "ExactlyOne" $ (+1) <$> ExactlyOne 2 @?= ExactlyOne 3

listTest :: MiniTestTree
listTest =
  testGroup "List" [
    testCase "empty list" $
      (+1) <$> Nil @?= Nil
  , testCase "increment" $
      (+1) <$> (1 :. 2 :. 3 :. Nil) @?= (2 :. 3 :. 4 :. Nil)
  ]

optionalTest :: MiniTestTree
optionalTest =
  testGroup "Optional" [
    testCase "Empty" $ (+1) <$> Empty @?= Empty
  , testCase "Full"  $ (+1) <$> Full 2 @?= Full 3
  ]

functionTest :: MiniTestTree
functionTest =
  testCase "(->)" $ ((+1) <$> (*2)) 8 @?= 17


anonMapTest :: MiniTestTree
anonMapTest =
  testGroup "(<$)" [
    testCase "Map 7" $ 7 <$ (1 :. 2 :. 3 :. Nil) @?= (7 :. 7 :. 7 :. Nil)
  , testProperty "Always maps a constant value over List" . Fn genInteger $
      \x -> Fn genInteger $
      \a -> Fn genInteger $
      \b -> fn genInteger $
      \c ->
        x <$ ((a :. b :. c :. Nil) :: List Integer) == (x :. x :. x :. Nil)
  , testProperty "Always maps a constant value over Full (Optional)" . Fn genInteger $
      \x -> fn genInteger $
      \q -> x <$ Full q == Full x
  ]

voidTest :: MiniTestTree
voidTest =
  testGroup "void" [
    testCase "List"  $ void (1 :. 2 :. 3 :. Nil) @?= () :. () :. () :. Nil
  , testCase "Full"  $ void (Full 7) @?= Full ()
  , testCase "Empty" $ void Empty @?= Empty
  , testCase "(->)"  $ void (+10) 5 @?= ()
  ]
