{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitPrelude        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}

-- | The smallest possible test library interface and instance that will run the current test suite.
-- For those comfortable with cabal, we also provide a Test.Tasty.Mini module with a type and
-- instance for running the test suite with Tasty.
module Test.Mini where

import           Control.Exception (SomeException, catch)
import           Data.Bool         (bool)
import           Data.Foldable     (traverse_)
import           Data.List         (intercalate)
import           Data.Monoid       ((<>))
import           Data.String       (IsString, fromString)

type MiniTestTree =
  forall t name assertion.
  ( UnitTester assertion
  , Tester t name assertion
  )
  => TestTree t

-- | Test interface required to run course tests
class IsString name => Tester t name assertion | t -> name, t -> assertion where
  data TestTree t

  testGroup :: name -> [TestTree t] -> TestTree t
  testCase :: name -> assertion -> TestTree t
  test :: TestTree t -> IO ()

class UnitTester assertion where
  (@?=) :: (Eq a, Show a) => a -> a -> assertion
  infix 1 @?=

class IsString name => PropertyTester t name prop | t -> name, t -> prop where
  testProperty :: name -> prop -> t


-- | A data type for our embedded instance to hang off
data CourseTester

-- | The test tree structure used by our embedded instance
data CourseTestTree =
  Single String Result
  | Tree String [CourseTestTree]

data Result =
  Failure String
  | Success
  deriving (Eq)

-- | Run our embedded test tree, printing failures.
testCourseTree' ::
  CourseTestTree -> IO ()
testCourseTree' =
  go ""
  where
    qualifiedName s s' =
      bool (intercalate "." [s,s']) s' (null s)
    go s (Single s' a) =
      let
        quote x = "'" <> x <> "'"

        printFailure :: Show e => e -> IO ()
        printFailure e =
          putStrLn (quote (qualifiedName s s') <> " failed:")
          >> print e
          >> putStrLn ""

        printResult (Failure e) = printFailure e
        printResult Success = pure ()
      in
        printResult a `catch`
          \(e :: SomeException) -> printFailure e
    go s (Tree s' ts) = foldMap (go (qualifiedName s s')) ts

-- | Instance for the embedded test implementation
instance Tester CourseTester String Result where
  data TestTree CourseTester = CTTree {unCTTree :: CourseTestTree}

  testGroup s =
    CTTree . Tree s . foldr ((:) . unCTTree) []

  testCase s a = CTTree (Single s a)

  test (CTTree t) =
    testCourseTree' t

instance UnitTester Result where
  a @?= b =
    let
      msg = "Expected " <> show a <> " but got " <> show b
    in
      bool (Failure msg) Success (a == b)


courseTest ::
  TestTree CourseTester
  -> IO ()
courseTest =
  test
