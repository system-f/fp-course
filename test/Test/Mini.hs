{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitPrelude        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

-- | The smallest possible test library interface and instance that will run the current test suite.
-- For those comfortable with cabal, we also provide a Test.Tasty.Mini module with a type and
-- instance for running the test suite with Tasty.
module Test.Mini where

import           Control.Exception (SomeException, try)
import           Data.Bool         (bool)
import           Data.Foldable     (traverse_)
import           Data.List         (intercalate)
import           Data.Monoid       ((<>))
import           Data.String       (IsString, fromString)

-- | Test interface required to run course tests
class IsString name => Tester t name | t -> name where
  data TestTree t
  data Assertion t

  testGroup :: name -> [TestTree t] -> TestTree t
  testCase :: name -> Assertion t -> TestTree t
  (@?=) :: (Eq a, Show a) => a -> a -> Assertion t
  test :: TestTree t -> IO [name]


-- | A data type for our embedded instance to hang off
data CourseTester

-- | The test tree structure used by our embedded instance
data CourseTestTree =
  Single { name :: String, assertion :: IO () }
  | Tree { name :: String, testCases :: [CourseTestTree]}

-- | Run our embedded test tree, printing failures.
testCourseTree' ::
  CourseTestTree -> IO [String]
testCourseTree' =
  go ""
  where
    go s t' =
      case t' of
      (Single s' a) -> do
        r <- try a :: IO (Either SomeException ())
        let
          qualifiedName = bool (intercalate "." [s,s']) s' (null s)
          quote x = "'" <> x <> "'"
          printFailure e =
            putStrLn (quote qualifiedName <> " failed:")
            >> print e
            >> putStrLn ""
        case r of
          Left e   -> printFailure e >> (pure . pure) qualifiedName
          Right () -> pure []
      (Tree s' ts) -> foldMap (go s') ts

-- | Instance for the embedded test implementation
instance Tester CourseTester String where
  data TestTree CourseTester = CTTree CourseTestTree
  data Assertion CourseTester = CTAssertion (IO ())

  testGroup s =
    foldr1 (\(CTTree t1) (CTTree t2) -> CTTree (Tree s [t1, t2]))

  testCase s (CTAssertion a) = CTTree (Single s a)

  a @?= b =
    let
      msg :: Prelude.String
      msg = "Expected " <> show a <> " but got " <> show b
    in
      CTAssertion $ bool (error msg) (pure ()) (a == b)

  test (CTTree t) =
    testCourseTree' t
