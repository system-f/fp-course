{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.MonadTest where

import           Test.Tasty        (TestTree, testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))

import           Course.Core
import           Course.ExactlyOne (ExactlyOne (..))
import           Course.List       (List (..))
import           Course.Monad      (join, (<**>), (=<<), (>>=), (<=<))
import           Course.Optional   (Optional (..))

test_Monad :: TestTree
test_Monad =
  testGroup "Monad" [
    appTest
  , bindExactlyOneTest
  , bindListTest
  , bindOptionalTest
  , bindReaderTest
  , joinTest
  , bindFlippedTest
  , kleisliCompositionTest
  ]

appTest :: TestTree
appTest =
  testGroup "<**>" [
    testCase "ExactlyOne" $
      ExactlyOne (+10) <**> ExactlyOne 8 @?= ExactlyOne 18
  , testCase "List" $
      (+1) :. (*2) :. Nil <**> 1 :. 2 :. 3 :. Nil @?= (2:.3:.4:.2:.4:.6:.Nil)
  , testCase "Optional" $
      Full (+8) <**> Full 7 @?= Full 15
  , testCase "Optional - empty function" $
      Empty <**> Full 7 @?= (Empty :: Optional Integer)
  , testCase "Optional - empty value" $
      Full (+8) <**> Empty @?= Empty
  , testCase "(->) 1" $
      ((+) <**> (+10)) 3 @?= 16
  , testCase "(->) 2" $
      ((+) <**> (+5)) 3 @?= 11
  , testCase "(->) 3" $
      ((+) <**> (+5)) 1 @?= 7
  , testCase "(->) 4" $
      ((*) <**> (+10)) 3 @?= 39
  , testCase "(->) 5" $
      ((*) <**> (+2)) 3 @?= 15
  ]

bindExactlyOneTest :: TestTree
bindExactlyOneTest =
  testCase "(=<<) for ExactlyOne" $
    ((\x -> ExactlyOne(x+1)) =<< ExactlyOne 2) @?= ExactlyOne 3

bindListTest :: TestTree
bindListTest =
  testCase "(=<<) for List" $
    ((\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)) @?= (1:.1:.2:.2:.3:.3:.Nil)

bindOptionalTest :: TestTree
bindOptionalTest =
  testCase "(=<<) for Optional" $
    ((\n -> Full (n + n)) =<< Full 7) @?= Full 14

bindReaderTest :: TestTree
bindReaderTest =
  testCase "(=<<) for (->)" $
    ((*) =<< (+10)) 7 @?= 119

joinTest :: TestTree
joinTest =
  testGroup "join" [
    testCase "List" $
      join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil) @?= (1:.2:.3:.1:.2:.Nil)
  , testCase "Optional with Empty" $
      join (Full Empty) @?= (Empty :: Optional Integer)
  , testCase "Optional all Full" $
      join (Full (Full 7)) @?= Full 7
  , testCase "(->)" $
      join (+) 7 @?= 14
  ]

bindFlippedTest :: TestTree
bindFlippedTest =
  testCase "(>>=)" $
    ((+10) >>= (*)) 7 @?= 119

kleisliCompositionTest :: TestTree
kleisliCompositionTest =
  testCase "kleislyComposition" $
    ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1 @?= (2:.2:.3:.3:.Nil)
