{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TestLoader
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
  , courseTest
  )
  where

import qualified Course.ApplicativeTest as Applicative
import qualified Course.ChequeTest      as ChequeTest
import qualified Course.ComonadTest     as Comonad
import qualified Course.ExtendTest      as Extend
import qualified Course.FunctorTest     as Functor
import qualified Course.JsonParserTest  as JsonParser
import qualified Course.ListTest        as List
import qualified Course.ListZipperTest  as ListZipper
import qualified Course.MonadTest       as Monad
import qualified Course.MoreParserTest  as MoreParser
import qualified Course.OptionalTest    as Optional
import qualified Course.ParserTest      as Parser
import qualified Course.StateTest       as State
import qualified Course.StateTTest      as StateT
import qualified Course.TraversableTest as Traversable
import qualified Course.ValidationTest  as Validation

import           Data.String            (fromString)

import           Test.Course.Mini       (courseTest)
import           Test.Mini              (MiniTestTree, testGroup)

tests :: MiniTestTree
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

