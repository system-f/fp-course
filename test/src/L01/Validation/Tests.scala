package L01

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import scalaz.{Validation => _, _}
import Scalaz._

class ValidationTests extends Specification with ScalaCheck {

  "Validation" >> {
    "isError is not equal to isValue" >> prop(isError_isValue)
    "valueOr produces or isValue"     >> prop(valueOr)
    "errorOr produces or isError"     >> prop(errorOr)
    "map maps"                        >> prop(map)
  }

  implicit def arbitraryValidationInt(implicit a: Arbitrary[Int]): Arbitrary[Validation[Int]] =
    Arbitrary(Gen.oneOf(arbitrary[Int] map (x => Value(x)),
      arbitrary[Int] map (x => Error(x.toString))))

  val isError_isValue: Validation[Int] => Boolean =
    x => x.isError /== x.isValue

  val valueOr: (Validation[Int], Int) => Boolean =
    (x, n) => x.isValue || x.valueOr(n) == n

  val errorOr: (Validation[Int], String) => Boolean =
    (x, e) => x.isError || x.errorOr(e) == e

  val map: (Validation[Int], Int => Int, Int) => Boolean =
    (x, f, n) => f(x.valueOr(n)) == x.map(f).valueOr(f(n))


}
