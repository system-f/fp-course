{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ListTest where

import           Control.Applicative   (liftA2, liftA3)
import qualified Prelude               as P (fmap, foldr, length)

import           Test.QuickCheck       (Arbitrary (..), Gen, forAllShrink)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import           Course.Core
import           Course.List           (List (..), filter, find, flatMap,
                                        flatten, flattenAgain, foldLeft, headOr,
                                        hlist, infinity, largeList, length,
                                        lengthGT4, listh, map, produce, product,
                                        reverse, seqOptional, sum, take, (++))
import           Course.Optional       (Optional (..))

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
  , testProperty "headOr on empty list always the default" $ \x -> x `headOr` Nil == (x :: Integer)
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
  , testProperty "subtracting each element in a list from its sum is always 0" $
      forAllShrink genList shrinkList (\x -> foldLeft (-) (sum x) x == 0)
  ]

lengthTest :: TestTree
lengthTest =
  testGroup "length" [
    testCase "length 1..3" $ length (1 :. 2 :. 3 :. Nil) @?= 3
  , testProperty "summing a list of 1s is equal to its length" $
      forAllShrink genIntegerList shrinkList (\x -> P.length (hlist x) == length x)
  ]

mapTest :: TestTree
mapTest =
  testGroup "map" [
    testCase "add 10 on list" $
      map (+10) (1 :. 2 :. 3 :. Nil) @?= (11 :. 12 :. 13 :. Nil)
  , testProperty "headOr after map" $
      \x -> headOr (x :: Integer) (map (+1) infinity) == 1
  , testProperty "map id is id" $
      forAllShrink genIntegerList shrinkList (\x -> map id x == x)
  ]

filterTest :: TestTree
filterTest =
  testGroup "filter" [
    testCase "filter even" $
      filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil) @?= (2 :. 4 :. Nil)
  , testProperty "filter (const True) is identity (headOr)" $
      \x -> headOr x (filter (const True) infinity) == 0
  , testProperty "filter (const True) is identity" $
      forAllShrink genIntegerList shrinkList (\x -> filter (const True) x == x)
  , testProperty "filter (const False) is the empty list" $
      forAllShrink genIntegerList shrinkList (\x -> filter (const False) x == Nil)
  ]

appendTest :: TestTree
appendTest =
  testGroup "(++)" [
    testCase "(1..6)" $
      (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil) @?= listh [1,2,3,4,5,6]
  , testProperty "append empty to infinite" $
      \x -> headOr x (Nil ++ infinity) == 0
  , testProperty "append anything to infinity" $
       forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (y ++ infinity) == headOr 0 y)
  , testProperty "associativity" $
      forAllShrink genThreeLists shrinkThreeLists (\(x,y,z) -> (x ++ y) ++ z == x ++ (y ++ z))
  , testProperty "append to empty list" $
      forAllShrink genIntegerList shrinkList (\x -> x ++ Nil == x)
  ]

flattenTest :: TestTree
flattenTest =
  testGroup "flatten" [
    testCase "(1..9)" $
      flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil) @?= listh [1,2,3,4,5,6,7,8,9]
  , testProperty "flatten (infinity :. y)" $
      forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (flatten (infinity :. y :. Nil)) == 0)
  , testProperty "flatten (y :. infinity)" $
      forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y)
  , testProperty "sum of lengths == length of flattened" $
      forAllShrink genListOfLists shrinkListOfLists (\x -> sum (map length x) == length (flatten x))
  ]

flatMapTest :: TestTree
flatMapTest =
  testGroup "flatMap" [
    testCase "lists of Integer" $
      flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil) @?= listh [1,2,3,2,3,4,3,4,5]
  , testProperty "flatMap id flattens a list of lists" $
      forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (flatMap id (infinity :. y :. Nil)) == 0)
  , testProperty "flatMap id on a list of lists take 2" $
      forAllShrink genIntegerAndList shrinkIntegerAndList (\(x, y) -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y)
  , testProperty "flatMap id == flatten" $
      forAllShrink genListOfLists shrinkListOfLists (\x -> flatMap id x == flatten x)
  ]

flattenAgainTest :: TestTree
flattenAgainTest =
  testGroup "flattenAgain" [
    testProperty "lists of Integer" $
      forAllShrink genListOfLists shrinkListOfLists (\x -> flatten x == flattenAgain x)
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
  , testProperty "reverse then append is same as append then reverse" $
      forAllShrink genTwoLists shrinkTwoLists (\(x, y) -> reverse x ++ reverse y == reverse (y ++ x))
  , testProperty "" $
      forAllShrink genIntegerList shrinkList (\x -> reverse (x :. Nil) == x :. Nil)
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

-- Use generator functions with `forAll` rather than orphans and/or newtype wrappers
genList :: Arbitrary a => Gen (List a)
genList = P.fmap ((P.foldr (:.) Nil) :: [a] -> List a) arbitrary

shrinkList :: Arbitrary a => List a -> [List a]
shrinkList =
  P.fmap listh . shrink . hlist

genIntegerList :: Gen (List Integer)
genIntegerList = genList

genIntegerAndList :: Gen (Integer, List Integer)
genIntegerAndList = P.fmap (P.fmap listh) arbitrary

shrinkIntegerAndList :: (Integer, List Integer) -> [(Integer, List Integer)]
shrinkIntegerAndList = P.fmap (P.fmap listh) . shrink . P.fmap hlist

genTwoLists :: Gen (List Integer, List Integer)
genTwoLists = liftA2 (,) genIntegerList genIntegerList -- (arbitrary :: (List Integer, List Integer))

shrinkTwoLists :: (List Integer, List Integer) -> [(List Integer, List Integer)]
shrinkTwoLists (a,b) = P.fmap (\(as,bs) -> (listh as, listh bs)) $ shrink (hlist a, hlist b)

genThreeLists :: Gen (List Integer, List Integer, List Integer)
genThreeLists = liftA3 (,,) genIntegerList genIntegerList genIntegerList

shrinkThreeLists :: (List Integer, List Integer, List Integer) -> [(List Integer, List Integer, List Integer)]
shrinkThreeLists (a,b,c) = P.fmap (\(as,bs,cs) -> (listh as, listh bs, listh cs)) $ shrink (hlist a, hlist b, hlist c)

genListOfLists :: Gen (List (List Integer))
genListOfLists = P.fmap (P.fmap listh) (genList :: (Gen (List [Integer])))

shrinkListOfLists :: Arbitrary a => List (List a) -> [List (List a)]
shrinkListOfLists = P.fmap (P.fmap listh). shrinkList . P.fmap hlist
