{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitPrelude       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Framework where

import           Control.Exception (SomeException, catch)
import           Data.Bool (bool)
import           Data.Foldable (traverse_)
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Test.Framework.Random (StdGen, mkStdGen, split)
import qualified Test.Framework.Property as P

data TestTree
  = Single String Result
  | Tree String [TestTree]

data Result
  = Failure String
  | Success
  deriving (Eq)

test :: TestTree -> IO ()
test = go ""
  where
    qualifiedName s s' =
      bool (intercalate "." [s,s']) s' (null s)

    indent n =
      unlines . fmap (replicate n ' ' <>) . lines

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

testGroup :: String -> [TestTree] -> TestTree
testGroup = Tree

testCase :: String -> Result -> TestTree
testCase = Single

testProperty :: P.Testable a => String -> a -> TestTree
testProperty label a = Single label $ go P.quick (P.evaluate a) (mkStdGen 0) 0 0
  where
    go :: P.Config -> P.Gen P.Result -> StdGen -> Int -> Int -> Result
    go config gen rnd0 ntest nfail
      | ntest == P.maxTest config = Success
      | ntest == P.maxFail config = Success -- XXX
      | otherwise = case P.ok res of
          Nothing -> go config gen rnd1 ntest (nfail + 1)
          Just True -> go config gen rnd1 (ntest + 1) nfail
          Just False -> Failure $ concat
            [ "Falsifiable after "
            , show ntest
            , " tests: "
            , intercalate ", " (P.arguments res)
            ]
      where
        res = P.generate (P.resize config ntest) rnd2 gen
        (rnd1, rnd2) = split rnd0

assertBool :: String -> Bool -> TestTree
assertBool name b = testCase name $ b @?= True

(@?=) :: (Eq a, Show a) => a -> a -> Result
a @?= b =
  let
    msg = "Expected " <> show b <> " but got " <> show a
  in
    bool (Failure msg) Success (a == b)

infix 1 @?=
