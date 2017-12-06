import Test.Tasty
import Course.ApplicativeTest (test_Applicative)
import Course.ComonadTest (test_Comonad)
import Course.ExtendTest (test_Extend)
import Course.FunctorTest (test_Functor)
import Course.JsonParserTest (test_JsonParser)
import Course.ListTest (test_List)
import Course.MonadTest (test_Monad)
import Course.OptionalTest (test_Optional)
import Course.StateTest (test_State)
import Course.StateTTest (test_StateT)
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
  , test_JsonParser
  ]

