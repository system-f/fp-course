{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- | The smallest possible test library that will run the current test suite
module Test.Mini where

import Data.Bool (bool)
import Data.Foldable (traverse_)

class Tester t where
  data TestName t
  data TestTree t
  data Assertion t

  testGroup :: TestName t -> [TestTree t] -> TestTree t
  testCase :: TestName t -> Assertion t -> TestTree t
  (@?=) :: (Eq a, Show a) => a -> a -> Assertion t
  test' :: TestTree t -> [TestName t]
  test :: TestTree t -> IO ()

data CourseTester =
  CourseTester

data CourseTestTree =
  Single { name :: String, assertion :: Bool }
  | Tree { name :: String, testCases :: [CourseTestTree]}

testCourseTree' ::
  CourseTestTree -> [String]
testCourseTree' t = case t of
  (Single s a) -> bool [s] [] a
  (Tree _ ts) -> foldMap testCourseTree' ts

instance Tester CourseTester where
  data TestName CourseTester = CTName String
  data TestTree CourseTester = CTTree CourseTestTree
  data Assertion CourseTester = CTAssertion Bool

  testGroup (CTName s) =
    foldr1 (\(CTTree t1) (CTTree t2) -> CTTree (Tree s [t1, t2]))

  testCase (CTName s) (CTAssertion a) = CTTree (Single s a)

  a @?= b =
    CTAssertion (a == b)

  test' (CTTree t) =
    CTName <$> testCourseTree' t

  test = traverse_ (\(CTName s) -> putStrLn s) . test'
