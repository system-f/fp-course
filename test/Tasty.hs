import           Test.Tasty

import           Course.ApplicativeTest
import           Course.FunctorTest
import           Course.ListTest
import           Course.ValidationTest

main :: IO ()
main =
  defaultMain $ testGroup "Course" [
    test_Validation
  , test_List
  , test_Functor
  , test_Applicative
  ]