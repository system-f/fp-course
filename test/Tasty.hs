import Test.Tasty

import Course.ValidationTest

main :: IO ()
main =
  defaultMain $ testGroup "Course" [
    test_Validation
  ]
