{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ImplicitPrelude        #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-- | The smallest possible test library interface and instance that will run the current test suite.
-- For those comfortable with cabal, we also provide a Test.Tasty.Mini module with a type and
-- instance for running the test suite with Tasty.
module Test.Mini where

import           Control.Exception (SomeException, catch)
import           Course.Validation (Validation (..))
import           Data.Bool         (bool)
import           Data.Foldable     (traverse_)
import           Data.List         (intercalate)
import           Data.Monoid       ((<>))
import           Data.String       (IsString, fromString)
import           GHC.Exts          (Constraint)

type MiniTestTree =
  forall name assertion t g.
  ( UnitTester t name assertion
  , Tester t name
  , PropertyTester t g name
  , Arbitrary t g
  )
  => t

-- | Test interface required to run course tests
class IsString name => Tester t name | t -> name where
  testGroup :: name -> [t] -> t
  test :: t -> IO ()

class IsString name => UnitTester t name assertion | t -> name, t -> assertion, assertion -> t where
  testCase :: name -> assertion -> t
  (@?=) :: (Eq a, Show a) => a -> a -> assertion
  infix 1 @?=

class IsString name => PropertyTester t g name | t -> name, t -> g where
  testProperty :: name -> Testable t g -> t

data Gen t a where
  GenInt :: Gen t Int
  GenString :: Gen t String
  GenMaybe :: Gen t a -> Gen t (Maybe a)
  GenA :: Gen t a -> (a -> b) -> (b -> [b]) -> Gen t b

class Arbitrary t (g :: * -> *) | g -> t, t -> g where
  gen :: Gen t a -> g a
  shrink :: Gen t a -> a -> [a]

data Testable t g where
  B :: Bool -> Testable t g
  Fn :: forall a t g. (Show a, Arbitrary t g) => Gen t a -> (a -> Testable t g) -> Testable t g

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
