{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Test.Course.Mini where

import Control.Exception (catch, SomeException)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.String (fromString)
import Data.Monoid ((<>))

import Test.Mini (Tester (..), UnitTester (..), Arbitrary (..), PropertyTester (..))

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
testCourseTree' t =
  go "" t >> putStrLn "WARNING: No properties tested"
  where
    qualifiedName s s' =
      bool (intercalate "." [s,s']) s' (null s)
    go s (Single s' a) =
      let
        quote x = "'" <> x <> "'"
        qName = quote (qualifiedName s s')

        printFailure :: Show e => e -> IO ()
        printFailure e =
          putStrLn ""
          >> putStrLn ("FAILED: " <> qName)
          >> putStrLn (indent 2 (show e))

        printResult (Failure e) = printFailure e
        printResult Success     = putStrLn $ "PASSED: " <> qName
      in
        printResult a `catch`
          \(e :: SomeException) -> printFailure e
    go s (Tree s' ts) = traverse_ (go (qualifiedName s s')) ts

indent ::
  Int
  -> String
  -> String
indent n =
  unlines . fmap (replicate n ' ' <>) . lines

-- | Instance for the embedded test implementation
instance Tester CourseTestTree String where
  testGroup s =
    Tree s . foldr (:) []
  test =
    testCourseTree'

instance UnitTester CourseTestTree String Result where
  testCase =
    Single

  a @?= b =
    let
      msg = "Expected " <> show b <> " but got " <> show a
    in
      bool (Failure msg) Success (a == b)

newtype CourseGen a =
  CourseGen a
  deriving (Functor)

instance Applicative CourseGen where
  pure = error "pure should never be called for CourseGen"
  (<*>) = error "(<*>) should never be called for CourseGen"

instance Monad CourseGen where
  (>>=) = error "(>>=) should never be called for CourseGen"

instance Arbitrary CourseTestTree CourseGen where
  gen = error "`gen` should never be called for CourseGen"
  shrink = error "`shrink` should never be called for CourseGen"

instance PropertyTester CourseTestTree CourseGen String where
  testProperty n = const (Tree n [])

courseTest ::
  CourseTestTree
  -> IO ()
courseTest =
  test
