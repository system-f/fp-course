{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ListTest (
  -- * Tests
    test_List
  , headOrTest
  , productTest
  , sumTest
  , lengthTest
  , mapTest
  , filterTest
  , appendTest
  , flattenTest
  , flatMapTest
  , flattenAgainTest
  , seqOptionalTest
  , findTest
  , lengthGT4Test
  , reverseTest
  , produceTest

  -- * Runner
  , test
  ) where

import qualified Prelude          as P (length)

import           Test.Framework   (TestTree, testCase, testGroup,
                                   testProperty, test, (@?=))

import           Course.Core
import           Course.List      (List ((:.), Nil), filter, find, flatMap,
                                   flatten, flattenAgain, foldLeft, headOr,
                                   hlist, infinity, largeList, length,
                                   lengthGT4, listh, map, produce, product,
                                   reverse, seqOptional, sum, take, (++))
import           Course.Optional  (Optional (Empty, Full))

test_List :: TestTree
test_List =
  testGroup "List" [
    headOrTest
  , productTest
  , sumTest
  , lengthTest
  , mapTest
  , filterTest
  , appendTest
  , flattenTest
  , flatMapTest
  , flattenAgainTest
  , seqOptionalTest
  , findTest
  , lengthGT4Test
  , reverseTest
  , produceTest
  ]

headOrTest :: TestTree
headOrTest =
  testGroup "headOr" [
    testCase "headOr on non-empty list" $ headOr 3 (1 :. 2 :. Nil) @?= 1
  , testCase "headOr on empty list" $ headOr 3 Nil @?= 3
  , testProperty "headOr on infinity always 0" $ \x -> x `headOr` infinity == 0
  , testProperty "headOr on empty list always the default" $ \x ->
      (x :: Integer) `headOr` Nil == x
  ]

productTest :: TestTree
productTest =
  testGroup "productTest" [
    testCase "product of empty list" $ product Nil @?= 1
  , testCase "product of 1..3" $ product (1 :. 2 :. 3 :. Nil) @?= 6
  , testCase "product of 1..4" $ product (1 :. 2 :. 3 :. 4 :. Nil) @?= 24
  ]

sumTest :: TestTree
sumTest =
  testGroup "sum" [
    testCase "sum 1..3" $ sum (1 :. 2 :. 3 :. Nil) @?= 6
  , testCase "sum 1..4" $ sum (1 :. 2 :. 3 :. 4 :. Nil) @?= 10
  , testProperty "subtracting each element in a list from its sum is always 0" $ \x ->
      foldLeft (-) (sum x) x == 0
  ]

lengthTest :: TestTree
lengthTest =
  testGroup "length" [
    testCase "length 1..3" $ length (1 :. 2 :. 3 :. Nil) @?= 3
  , testProperty "summing a list of 1s is equal to its length" $ \x ->
      P.length (hlist x) == length (x :: List Integer)
  ]

mapTest :: TestTree
mapTest =
  testGroup "map" [
    testCase "add 10 on list" $
      map (+10) (1 :. 2 :. 3 :. Nil) @?= (11 :. 12 :. 13 :. Nil)
  , testProperty "headOr after map" $ \x ->
      headOr (x :: Integer) (map (+1) infinity) == 1
  , testProperty "map id is id" $ \x ->
      map id x == (x :: List Integer)
  ]

filterTest :: TestTree
filterTest =
  testGroup "filter" [
    testCase "filter even" $
      filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) @?= (2 :. 4 :. Nil)
  , testProperty "filter (const True) is identity (headOr)" $ \x ->
      headOr x (filter (const True) infinity) == 0
  , testProperty "filter (const True) is identity" $ \x ->
      filter (const True) x == (x :: List Integer)
  , testProperty "filter (const False) is the empty list" $ \x ->
      filter (const False) x == (Nil :: List Integer)
  ]

appendTest :: TestTree
appendTest =
  testGroup "(++)" [
    testCase "(1..6)" $
      (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil) @?= listh [1,2,3,4,5,6]
  , testProperty "append empty to infinite" $
      \x -> headOr x (Nil ++ infinity) == 0
  , testProperty "append anything to infinity" $
      \x y -> headOr x (y ++ infinity) == headOr 0 y
  , testProperty "associativity" $
      \x y z -> (x ++ y) ++ z == (x ++ (y ++ z) :: List Integer)
  , testProperty "append to empty list" $
      \x -> x ++ Nil == (x :: List Integer)
  ]

flattenTest :: TestTree
flattenTest =
  testGroup "flatten" [
    testCase "(1..9)" $
      flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) @?= listh [1,2,3,4,5,6,7,8,9]
  , testProperty "flatten (infinity :. y)" $ \(x, y) ->
      headOr x (flatten (infinity :. y :. Nil)) == 0
  , testProperty "flatten (y :. infinity)" $ \(x, y) ->
      headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
  , testProperty "sum of lengths == length of flattened" $ \x ->
      sum (map length x) == length (flatten (x :: List (List Integer)))
  ]

flatMapTest :: TestTree
flatMapTest =
  testGroup "flatMap" [
    testCase "lists of Integer" $
      flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil) @?= listh [1,2,3,2,3,4,3,4,5]
  , testProperty "flatMap id flattens a list of lists" $ \x y ->
      headOr x (flatMap id (infinity :. y :. Nil)) == 0
  , testProperty "flatMap id on a list of lists take 2" $ \x y ->
      headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
  , testProperty "flatMap id == flatten" $ \x ->
      flatMap id x == flatten (x :: List (List Integer))
  ]

flattenAgainTest :: TestTree
flattenAgainTest =
  testGroup "flattenAgain" [
    testProperty "lists of Integer" $ \x ->
      flatten x == flattenAgain (x :: List (List Integer))
  ]


seqOptionalTest :: TestTree
seqOptionalTest =
  testGroup "seqOptional" [
    testCase "all Full" $
      seqOptional (Full 1 :. Full 10 :. Nil) @?= Full (1 :. 10 :. Nil)
  , testCase "empty list" $
      let empty = Nil :: List (Optional Integer)
       in seqOptional empty @?= Full Nil
  , testCase "contains Empty" $
      seqOptional (Full 1 :. Full 10 :. Empty :. Nil) @?= Empty
  , testCase "Empty at head of infinity" $
      seqOptional (Empty :. map Full infinity) @?= Empty
  ]

findTest :: TestTree
findTest =
  testGroup "find" [
    testCase "find no matches" $
      find even (1 :. 3 :. 5 :. Nil) @?= Empty
  , testCase "empty list" $ find even Nil @?= Empty
  , testCase "find only even" $
      find even (1 :. 2 :. 3 :. 5 :. Nil) @?= Full 2
  , testCase "find first, not second even" $
      find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) @?= Full 2
  , testCase "find on infinite list" $
      find (const True) infinity @?= Full 0
  ]

lengthGT4Test :: TestTree
lengthGT4Test =
  testGroup "lengthGT4" [
    testCase "list of length 3" $
      lengthGT4 (1 :. 3 :. 5 :. Nil) @?= False
  , testCase "list of length 4" $
      lengthGT4 (1 :. 2 :. 3 :. 4 :. Nil) @?= False
  , testCase "empty list" $
      lengthGT4 Nil @?= False
  , testCase "list of length 5" $
      lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil) @?= True
  , testCase "infinite list" $
      lengthGT4 infinity @?= True
  ]

reverseTest :: TestTree
reverseTest =
  testGroup "reverse" [
    testCase "empty list" $
      reverse Nil @?= (Nil :: List Integer)
  , testCase "reverse . reverse on largeList" $
      take 1 (reverse (reverse largeList)) @?= (1 :. Nil)
  , testProperty "reverse then append is same as append then reverse" $ \x y ->
      reverse x ++ reverse y == (reverse (y ++ x) :: List Integer)
  , testProperty "reverse single element list is the list" $ \x ->
      reverse (x :. Nil) == (x :. Nil :: List Integer)
  ]

produceTest :: TestTree
produceTest =
  testGroup "produce" [
    testCase "increment" $
      let (x:.y:.z:.w:._) = produce (+1) 0
       in (x:.y:.z:.w:.Nil) @?= (0:.1:.2:.3:.Nil)
  , testCase "double" $
      let (x:.y:.z:.w:._) = produce (*2) 1
       in (x:.y:.z:.w:.Nil) @?= (1:.2:.4:.8:.Nil)
  ]
