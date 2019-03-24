{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ImplicitPrelude        #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}

-- | The smallest possible test library interface that will run the current test
-- suite. For those comfortable with cabal, we also provide a Test.Tasty.Mini
-- module with a type and instance for running the test suite with Tasty.
module Test.Mini where

import           Data.String (IsString)

type MiniTestTree =
  forall name assertion t g.
  ( UnitTester t name assertion
  , Tester t name
  , PropertyTester t g name
  , Arbitrary t g
  )
  => t

-- | Building and running a test tree.
class IsString name => Tester t name | t -> name where
  testGroup :: name -> [t] -> t
  test :: t -> IO ()

-- | Adding test cases.
class IsString name => UnitTester t name assertion | t -> name, t -> assertion, assertion -> t where
  testCase :: name -> assertion -> t

  (@?=) :: (Eq a, Show a) => a -> a -> assertion
  infix 1 @?=

  assertBool :: name -> Bool -> t
  assertBool name b = testCase name $ b @?= True

-- | Embed a property in a test tree
class IsString name => PropertyTester t g name | t -> name, t -> g where
  testProperty :: name -> Testable t g -> t

-- | Abstract representation of properties.
data Testable t g where
  B :: Bool -> Testable t g
  Fn :: forall a t g. (Show a, Arbitrary t g) => Gen t a -> (a -> Testable t g) -> Testable t g

-- | Helper to make specifying properties a little nicer.
--
-- >>> testProperty "succ" . fn GenInt $ \n -> n < succ n
fn ::
  forall t g a.
  (Arbitrary t g, Show a)
  => Gen t a
  -> (a -> Bool)
  -> Testable t g
fn g =
  Fn g . (B .)

-- | Generation and shrinking of values for property based tests.
class Arbitrary t (g :: * -> *) | g -> t, t -> g where
  gen :: Gen t a -> g a
  shrink :: Gen t a -> a -> [a]

-- | Abstract generators.
data Gen t a where
  GenBool :: Gen t Bool
  GenInt :: Gen t Int
  GenString :: Gen t String
  GenMaybe :: Gen t a -> Gen t (Maybe a)
  GenList :: Gen t a -> Gen t [a]
  GenA :: Gen t a -> (a -> b) -> (b -> [b]) -> Gen t b
  GenAB :: Gen t a -> Gen t b -> (a -> b -> c) -> (c -> [c]) -> Gen t c
