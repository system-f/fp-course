{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Data.String            (fromString)

import           Test.Tasty.Mini        (tastyTest)
import           TestLoader             (tests)

import qualified Course.ApplicativeTest as ApplicativeTest
import qualified Course.ComonadTest     as ComonadTest
import qualified Course.ExtendTest      as ExtendTest
import qualified Course.FunctorTest     as FunctorTest
import qualified Course.JsonParserTest  as JsonParserTest
import qualified Course.ListTest        as ListTest
import qualified Course.ListZipperTest  as ListZipperTest
import qualified Course.MonadTest       as MonadTest
import qualified Course.OptionalTest    as OptionalTest
import qualified Course.StateTest       as StateTest
import qualified Course.StateTTest      as StateTTest
import qualified Course.ValidationTest  as ValidationTest

import           Prelude                (IO)

main :: IO ()
main = tastyTest tests

