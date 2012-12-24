package L01

import org.scalatest.FeatureSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.oneOf

import scalaz._
import Scalaz._

class ValidationTests extends FeatureSpec with Checkers {

  implicit def arbitraryValidationInt(implicit a: Arbitrary[Int]): Arbitrary[Validation[Int]] =
    Arbitrary(oneOf(arbitrary[Int] map (x => Value(x)),
                    arbitrary[Int] map (x => Error(x.toString))))

  feature("Validation") {
    scenario("isError is not equal to isValue") (check(prop_isError_isValue))
    scenario("valueOr produces or isValue") (check(prop_valueOr))
    scenario("errorOr produces or isError") (check(prop_errorOr))
    scenario("map maps") (check(prop_map))
  }

  val prop_isError_isValue: Validation[Int] => Boolean =
    x => x.isError /== x.isValue

  val prop_valueOr: (Validation[Int], Int) => Boolean =
    (x, n) => x.isValue || x.valueOr(n) == n

  val prop_errorOr: (Validation[Int], String) => Boolean =
    (x, e) => x.isError || x.errorOr(e) == e

  val prop_map: (Validation[Int], Int => Int, Int) => Boolean =
    (x, f, n) => f(x.valueOr(n)) == x.map(f).valueOr(f(n))

}
