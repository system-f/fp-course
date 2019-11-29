{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Loader
  ( Optional.test_Optional
  , List.test_List
  , Functor.test_Functor
  , Applicative.test_Applicative
  , Monad.test_Monad
  , State.test_State
  , StateT.test_StateT
  , Validation.test_Validation
  , Extend.test_Extend
  , Comonad.test_Comonad
  , Traversable.test_Traversable
  , ListZipper.test_ListZipper
  , Parser.test_Parser
  , MoreParser.test_MoreParser
  , JsonParser.test_JsonParser
  , ChequeTest.test_Cheque

  , tests
  )
  where

import qualified Test.ApplicativeTest as Applicative
import qualified Test.ChequeTest      as ChequeTest
import qualified Test.ComonadTest     as Comonad
import qualified Test.ExtendTest      as Extend
import qualified Test.FunctorTest     as Functor
import qualified Test.JsonParserTest  as JsonParser
import qualified Test.ListTest        as List
import qualified Test.ListZipperTest  as ListZipper
import qualified Test.MonadTest       as Monad
import qualified Test.MoreParserTest  as MoreParser
import qualified Test.OptionalTest    as Optional
import qualified Test.ParserTest      as Parser
import qualified Test.StateTest       as State
import qualified Test.StateTTest      as StateT
import qualified Test.TraversableTest as Traversable
import qualified Test.ValidationTest  as Validation

import           Data.String            (fromString)

import           Test.Framework         (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup "Tests" [
    Optional.test_Optional
  , List.test_List
  , Functor.test_Functor
  , Applicative.test_Applicative
  , Monad.test_Monad
  , State.test_State
  , StateT.test_StateT
  , Validation.test_Validation
  , Extend.test_Extend
  , Comonad.test_Comonad
  , Traversable.test_Traversable
  , ListZipper.test_ListZipper
  , Parser.test_Parser
  , MoreParser.test_MoreParser
  , JsonParser.test_JsonParser
  , ChequeTest.test_Cheque
  ]
