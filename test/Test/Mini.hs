{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitPrelude        #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds #-}
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
import GHC.Exts (Constraint)

type MiniTestTree t g =
  forall name assertion.
  ( UnitTester t name assertion
  , Tester t name
  , PropertyTester t g name
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

class (Monad g) => Gen t (g :: * -> *) a | g -> t, t -> g where
  gen :: g a
  shrink :: p t -> a -> [a]

data Testable t g where
  B :: Bool -> Testable t g
  Fn :: forall a t g. (Show a, Gen t g a) => (a -> Testable t g) -> Testable t g

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
    go s (Tree s' ts) = traverse_ (go (qualifiedName s s')) ts

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
      msg = "Expected " <> show a <> " but got " <> show b
    in
      bool (Failure msg) Success (a == b)

newtype CourseGen a =
  CourseGen a
  deriving (Functor)

instance Applicative CourseGen where
  pure = error "pure for CourseGen should never be called"
  (<*>) = error "(<*>) for CourseGen should never be called"

instance Monad CourseGen where
  (>>=) = error "(>>=) for CourseGen should never be called"

instance Gen CourseTestTree CourseGen a where
  gen = undefined
  shrink = undefined

instance PropertyTester CourseTestTree CourseGen String where
  testProperty n = const (Tree n [])

courseTest ::
  CourseTestTree
  -> IO ()
courseTest =
  test
