{-# LANGUAGE ImplicitPrelude #-}

import Data.String (fromString)
import Test.Tasty
import Course.ApplicativeTest (test_Applicative)
import Course.ComonadTest (test_Comonad)
import Course.ExtendTest (test_Extend)
import Course.FunctorTest (test_Functor)
import Course.JsonParserTest (test_JsonParser)
import Course.ChequeTest (test_Cheque)
import Course.ListTest (test_List)
import Course.ListZipperTest (test_ListZipper)
import Course.MonadTest (test_Monad)
import Course.OptionalTest (test_Optional)
import Course.StateTest (test_State)
import Course.StateTTest (test_StateT)
import Course.TraversableTest (test_Traversable)
import Course.ValidationTest (test_Validation)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests" [
    test_Optional
  , test_List
  , test_Functor
  , test_Applicative
  , test_Monad
  , test_State
  , test_StateT
  , test_Validation
  , test_Extend
  , test_Comonad
  , test_Traversable
  , test_ListZipper
  , test_JsonParser
  , test_Cheque
  ]

