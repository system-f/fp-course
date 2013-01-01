package L02

import org.scalatest.FeatureSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.oneOf

import List._

class ListTests extends FeatureSpec with Checkers {

  implicit def arbitraryListInt(implicit a: Arbitrary[Int]): Arbitrary[List[Int]] =
    Arbitrary(oneOf(arbitrary[Int] map (x => x |: Nil()),
                    arbitrary[Int] map (x => Nil())))

  implicit def arbitraryListListInt(implicit a: Arbitrary[Int]): Arbitrary[List[List[Int]]] =
    Arbitrary(arbitraryListInt.arbitrary.map(x => x |: Nil()))

  feature("List") {
    scenario("headOr defaults with Nil") (check(prop_headOr_Nil))
    scenario("headOr uses head with non-empty") (check(prop_headOr_Cons))
    scenario("sum reduces to zero with subtraction") (check(prop_suum))
    scenario("length reduces to zero with subtraction") (check(prop_len))
    scenario("map obeys law of identity") (check(prop_map_identity))
    scenario("map obeys law of composition") (check(prop_map_composition))
    scenario("filter leaves only valid values") (check(prop_fiilter))
    scenario("append produces a list with the sum of the lengths") (check(prop_append))
    scenario("flatten sums the lengths") (check(prop_flatten))
    scenario("flatMap obeys law of left identity") (check(prop_flatMap_left_identity))
    scenario("flatMap obeys law of right identity") (check(prop_flatMap_right_identity))
    scenario("flatMap obeys law of associativity") (check(prop_flatMap_associativity))
    scenario("flatMap with id flattens") (check(prop_flatMap_id_flattens))
    scenario("flatMap obeys functor relationship") (check(prop_flatMap_functor))
    scenario("rev with single value") (check(prop_rev_single_value))
    scenario("appending reverse is equal to reversing appended") (check(prop_rev_append))
  }

  val prop_headOr_Nil: Int => Boolean =
    x => Nil[Int]().headOr(x) == x

  val prop_headOr_Cons: (Int, List[Int], Int) => Boolean =
    (h, t, x) => (h |: t).headOr(x) == h

  val prop_suum: List[Int] => Boolean =
    x => x.foldLeft[Int](a => b => a - b, sum(x)) == 0

  val prop_len: List[Int] => Boolean =
    x => x.foldLeft[Int](a => b => a - 1, x.len) == 0

  val prop_map_identity: List[Int] => Boolean =
    x => x.map(identity) == x

  val prop_map_composition: ((Int => Int), (Int => Int), List[Int]) => Boolean =
    (f, g, x) => x.map(g).map(f) == x.map(f compose g)

  val not: Boolean => Boolean = x => !x

  val prop_fiilter: ((Int => Boolean), List[Int]) => Boolean =
    (p, x) => x.filter(p).foldRight[Boolean](a => b => p(a) && b, true) &&
              x.filter(not compose p).foldRight[Boolean](a => b => not (p(a) && b), true)

  val prop_append: (List[Int], List[Int]) => Boolean =
    (x, y) => x.len + y.len == (x append y).len

  val prop_flatten: List[List[Int]] => Boolean =
    x => flatten(x).len == sum(x.map(_.len))

  val prop_flatMap_right_identity: List[Int] => Boolean =
    x => x.flatMap(n => n |: Nil()) == x

  val prop_flatMap_left_identity: ((Int => List[Int]), Int) => Boolean =
    (f, n) => (n |: Nil()).flatMap(f) == f(n)

  val prop_flatMap_associativity: ((Int => List[Int]), (Int => List[Int]), List[Int]) => Boolean =
    (f, g, x) => x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

  val prop_flatMap_id_flattens: List[List[Int]] => Boolean =
    x => x.flatMap(identity) == flatten(x)

  val prop_flatMap_functor: ((Int => Int), List[Int]) => Boolean =
    (f, x) => x.map(f) == x.flatMap(w => f(w) |: Nil())

  val prop_rev_single_value: Int => Boolean =
    n => (n |: Nil()).rev == (n |: Nil())

  val prop_rev_append: (List[Int], List[Int]) => Boolean =
    (x, y) => ((x.rev) append (y.rev)) == y.append(x).rev

}
