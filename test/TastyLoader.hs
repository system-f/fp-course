{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Data.String            (fromString)
import           Test.Tasty.Mini        (tastyTest)
import           TestLoader             (tests)

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

import           Prelude                (IO)

main :: IO ()
main = tastyTest tests
