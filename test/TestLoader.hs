{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , ListZipper.test_ListZipper
  , JsonParser.test_JsonParser

  , tests
  , courseTest
  )
  where

import qualified Course.ApplicativeTest as Applicative
import qualified Course.ComonadTest     as Comonad
import qualified Course.ExtendTest      as Extend
import qualified Course.FunctorTest     as Functor
import qualified Course.JsonParserTest  as JsonParser
import qualified Course.ListTest        as List
import qualified Course.ListZipperTest  as ListZipper
import qualified Course.MonadTest       as Monad
import qualified Course.OptionalTest    as Optional
import qualified Course.StateTest       as State
import qualified Course.StateTTest      as StateT
import qualified Course.ValidationTest  as Validation

import           Data.String            (fromString)

import           Test.Course.Mini       (courseTest)
import           Test.Mini              (MiniTestTree, Tester (..))

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
  , ListZipper.test_ListZipper
  , JsonParser.test_JsonParser
  ]

