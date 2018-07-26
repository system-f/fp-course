import Data.String (fromString)

import Test.Tasty.Mini (tastyTest)
import TestLoader (tests)

import Prelude (IO)

main :: IO ()
main = tastyTest tests

