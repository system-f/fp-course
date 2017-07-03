import           Test.Tasty             (defaultMain, testGroup)

import           Course.ApplicativeTest (test_Applicative)
import           Course.FunctorTest     (test_Functor)
import           Course.JsonParserTest  (test_JsonParser)
import           Course.ListTest        (test_List)
import           Course.MonadTest       (test_Monad)
import           Course.OptionalTest    (test_Optional)
import           Course.ValidationTest  (test_Validation)

main :: IO ()
main =
  defaultMain $ testGroup "Course" [
    test_Validation
  , test_Optional
  , test_List
  , test_Functor
  , test_Applicative
  , test_Monad
  , test_JsonParser
  ]
