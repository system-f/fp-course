package L02

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import List._

class ListTests extends Specification with ScalaCheck {

  "List" >> {
    "headOr defaults with Nil"                          >> prop(headOr_Nil)
    "headOr uses head with non-empty"                   >> prop(headOr_Cons)
    "sum reduces to zero with subtraction"              >> prop(suum)
    "length reduces to zero with subtraction"           >> prop(len)
    "map obeys law of identity"                         >> prop(map_identity)
    "map obeys law of composition"                      >> prop(map_composition)
    "filter leaves only valid values"                   >> prop(fiilter)
    "append produces a list with the sum of the lengths">> prop(append)
    "flatten sums the lengths"                          >> prop(flattenList)
    "flatMap obeys law of left identity"                >> prop(flatMap_left_identity)
    "flatMap obeys law of right identity"               >> prop(flatMap_right_identity)
    "flatMap obeys law of associativity"                >> prop(flatMap_associativity)
    "flatMap with id flattens"                          >> prop(flatMap_id_flattens)
    "flatMap obeys functor relationship"                >> prop(flatMap_functor)
    "rev with single value"                             >> prop(rev_single_value)
    "appending reverse is equal to reversing appended"  >> prop(rev_append)
  }

  implicit def arbitraryListInt(implicit a: Arbitrary[Int]): Arbitrary[List[Int]] =
    Arbitrary(Gen.oneOf(arbitrary[Int] map (x => x |: Nil()),
      arbitrary[Int] map (x => Nil())))

  implicit def arbitraryListListInt(implicit a: Arbitrary[Int]): Arbitrary[List[List[Int]]] =
    Arbitrary(arbitraryListInt.arbitrary.map(x => x |: Nil()))

  val headOr_Nil: Int => Boolean =
    x => Nil[Int]().headOr(x) == x

  val headOr_Cons: (Int, List[Int], Int) => Boolean =
    (h, t, x) => (h |: t).headOr(x) == h

  val suum: List[Int] => Boolean =
    x => x.foldLeft[Int](a => b => a - b, sum(x)) == 0

  val len: List[Int] => Boolean =
    x => x.foldLeft[Int](a => b => a - 1, x.len) == 0

  val map_identity: List[Int] => Boolean =
    x => x.map(identity) == x

  val map_composition: ((Int => Int), (Int => Int), List[Int]) => Boolean =
    (f, g, x) => x.map(g).map(f) == x.map(f compose g)

  val fiilter: ((Int => Boolean), List[Int]) => Boolean =
    (p, x) => x.filter(p).foldRight[Boolean](a => b => p(a) && b, true) &&
              x.filter((!(_:Boolean)) compose p).foldRight[Boolean](a => b => !(p(a) && b), true)

  val append: (List[Int], List[Int]) => Boolean =
    (x, y) => x.len + y.len == (x append y).len

  val flattenList: List[List[Int]] => Boolean =
    x => flatten(x).len == sum(x.map(_.len))

  val flatMap_right_identity: List[Int] => Boolean =
    x => x.flatMap(n => n |: Nil()) == x

  val flatMap_left_identity: ((Int => List[Int]), Int) => Boolean =
    (f, n) => (n |: Nil()).flatMap(f) == f(n)

  val flatMap_associativity: ((Int => List[Int]), (Int => List[Int]), List[Int]) => Boolean =
    (f, g, x) => x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

  val flatMap_id_flattens: List[List[Int]] => Boolean =
    x => x.flatMap(identity) == flatten(x)

  val flatMap_functor: ((Int => Int), List[Int]) => Boolean =
    (f, x) => x.map(f) == x.flatMap(w => f(w) |: Nil())

  val rev_single_value: Int => Boolean =
    n => (n |: Nil()).rev == (n |: Nil())

  val rev_append: (List[Int], List[Int]) => Boolean =
    (x, y) => ((x.rev) append (y.rev)) == y.append(x).rev

}
