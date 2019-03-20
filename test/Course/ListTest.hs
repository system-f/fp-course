{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.ListTest (
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

  -- * Course test runner
  , courseTest
  ) where

import qualified Prelude          as P (length)

import           Test.Course.Mini (courseTest)
import           Test.Mini        (Gen (GenInt), MiniTestTree, Testable (B, Fn),
                                   fn, testCase, testGroup, testProperty, (@?=))

import           Course.Core
import           Course.Gens      (genInteger, genIntegerAndList, genList,
                                   genThreeLists, genTwoLists)
import           Course.List      (List ((:.), Nil), filter, find, flatMap,
                                   flatten, flattenAgain, foldLeft, headOr,
                                   hlist, infinity, largeList, length,
                                   lengthGT4, listh, map, produce, product,
                                   reverse, seqOptional, sum, take, (++))
import           Course.Optional  (Optional (Empty, Full))

test_List :: MiniTestTree
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

headOrTest :: MiniTestTree
headOrTest =
  testGroup "headOr" [
    testCase "headOr on non-empty list" $ headOr 3 (1 :. 2 :. Nil) @?= 1
  , testCase "headOr on empty list" $ headOr 3 Nil @?= 3
  , testProperty "headOr on infinity always 0" . Fn genInteger $ \x -> B $ x `headOr` infinity == 0
  , testProperty "headOr on empty list always the default" . fn genInteger $ \x -> x `headOr` Nil == x
  ]

productTest :: MiniTestTree
productTest =
  testGroup "productTest" [
    testCase "product of empty list" $ product Nil @?= 1
  , testCase "product of 1..3" $ product (1 :. 2 :. 3 :. Nil) @?= 6
  , testCase "product of 1..4" $ product (1 :. 2 :. 3 :. 4 :. Nil) @?= 24
  ]

sumTest :: MiniTestTree
sumTest =
  testGroup "sum" [
    testCase "sum 1..3" $ sum (1 :. 2 :. 3 :. Nil) @?= 6
  , testCase "sum 1..4" $ sum (1 :. 2 :. 3 :. 4 :. Nil) @?= 10
  , testProperty "subtracting each element in a list from its sum is always 0" . fn (genList GenInt) $
      \x -> foldLeft (-) (sum x) x == 0
  ]

lengthTest :: MiniTestTree
lengthTest =
  testGroup "length" [
    testCase "length 1..3" $ length (1 :. 2 :. 3 :. Nil) @?= 3
  , testProperty "summing a list of 1s is equal to its length" . fn (genList GenInt) $
      \x -> P.length (hlist x) == length x
  ]

mapTest :: MiniTestTree
mapTest =
  testGroup "map" [
    testCase "add 10 on list" $
      map (+10) (1 :. 2 :. 3 :. Nil) @?= (11 :. 12 :. 13 :. Nil)
  , testProperty "headOr after map" . fn genInteger $
      \x -> headOr (x :: Integer) (map (+1) infinity) == 1
  , testProperty "map id is id" . fn (genList genInteger) $
      \x -> map id x == x
  ]

filterTest :: MiniTestTree
filterTest =
  testGroup "filter" [
    testCase "filter even" $
      filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) @?= (2 :. 4 :. Nil)
  , testProperty "filter (const True) is identity (headOr)" . fn genInteger $
      \x -> headOr x (filter (const True) infinity) == 0
  , testProperty "filter (const True) is identity" . fn (genList genInteger) $
      (\x -> filter (const True) x == x)
  , testProperty "filter (const False) is the empty list" . fn (genList genInteger) $
      \x -> filter (const False) x == Nil
  ]

appendTest :: MiniTestTree
appendTest =
  testGroup "(++)" [
    testCase "(1..6)" $
      (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil) @?= listh [1,2,3,4,5,6]
  , testProperty "append empty to infinite" . fn genInteger $
      \x -> headOr x (Nil ++ infinity) == 0
  , testProperty "append anything to infinity" . fn genIntegerAndList $
       \(x, y) -> headOr x (y ++ infinity) == headOr 0 y
  , testProperty "associativity" . fn genThreeLists $
      \(x,y,z) -> (x ++ y) ++ z == x ++ (y ++ z)
  , testProperty "append to empty list" . fn (genList genInteger) $
      \x -> x ++ Nil == x
  ]

flattenTest :: MiniTestTree
flattenTest =
  testGroup "flatten" [
    testCase "(1..9)" $
      flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) @?= listh [1,2,3,4,5,6,7,8,9]
  , testProperty "flatten (infinity :. y)" . fn genIntegerAndList $
      \(x, y) -> headOr x (flatten (infinity :. y :. Nil)) == 0
  , testProperty "flatten (y :. infinity)" . fn genIntegerAndList $
      \(x, y) -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
  , testProperty "sum of lengths == length of flattened" . fn (genList (genList genInteger)) $
      \x -> sum (map length x) == length (flatten x)
  ]

flatMapTest :: MiniTestTree
flatMapTest =
  testGroup "flatMap" [
    testCase "lists of Integer" $
      flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil) @?= listh [1,2,3,2,3,4,3,4,5]
  , testProperty "flatMap id flattens a list of lists" . fn genIntegerAndList $
      \(x, y) -> headOr x (flatMap id (infinity :. y :. Nil)) == 0
  , testProperty "flatMap id on a list of lists take 2" . fn genIntegerAndList $
      \(x, y) -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
  , testProperty "flatMap id == flatten" . fn (genList (genList genInteger)) $
      \x -> flatMap id x == flatten x
  ]

flattenAgainTest :: MiniTestTree
flattenAgainTest =
  testGroup "flattenAgain" [
    testProperty "lists of Integer" . fn (genList (genList genInteger)) $
      \x -> flatten x == flattenAgain x
  ]


seqOptionalTest :: MiniTestTree
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

findTest :: MiniTestTree
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

lengthGT4Test :: MiniTestTree
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

reverseTest :: MiniTestTree
reverseTest =
  testGroup "reverse" [
    testCase "empty list" $
      reverse Nil @?= (Nil :: List Integer)
  , testCase "reverse . reverse on largeList" $
      take 1 (reverse (reverse largeList)) @?= (1 :. Nil)
  , testProperty "reverse then append is same as append then reverse" . fn genTwoLists $
      \(x, y) -> reverse x ++ reverse y == reverse (y ++ x)
  , testProperty "reverse single element list is the list" . fn genInteger $
      \x -> reverse (x :. Nil) == x :. Nil
  ]

produceTest :: MiniTestTree
produceTest =
  testGroup "produce" [
    testCase "increment" $
      let (x:.y:.z:.w:._) = produce (+1) 0
       in (x:.y:.z:.w:.Nil) @?= (0:.1:.2:.3:.Nil)
  , testCase "double" $
      let (x:.y:.z:.w:._) = produce (*2) 1
       in (x:.y:.z:.w:.Nil) @?= (1:.2:.4:.8:.Nil)
  ]
