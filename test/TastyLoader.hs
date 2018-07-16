import Test.Tasty.Mini (tastyTest)
-- import TestLoader (tests)
-- import Course.ApplicativeTest (test_Applicative)
-- import Course.ComonadTest (test_Comonad)
-- import Course.ExtendTest (test_Extend)
-- import Course.FunctorTest (test_Functor)
-- import Course.JsonParserTest (test_JsonParser)
-- import Course.ListTest (test_List)
-- import Course.ListZipperTest (test_ListZipper)
-- import Course.MonadTest (test_Monad)
-- import Course.OptionalTest (test_Optional)
-- import Course.StateTest (test_State)
-- import Course.StateTTest (test_StateT)
import Course.ValidationTest (test_Validation)

import Prelude (IO)

main :: IO ()
main = tastyTest test_Validation

