package L03

import L01._
import L02._

trait Moonad[M[_]] {
  def bind[A, B](f: A => M[B]): M[A] => M[B]
  def reeturn[A]: A => M[A]

  // Exercise 4
  // Relative Difficulty: 3
  // (use bind and reeturn)
  final def fmaap[A, B](f: A => B): M[A] => M[B] =
    sys.error("todo")

  // Provided as a synonym for `fmaap` but with the arguments flipped.
  // This helps the type-inferencer.
  final def apply[A, B](a: M[A])(f: A => B): M[B] =
    fmaap(f)(a)

  // Provided as a synonym for `bind` but with the arguments flipped.
  // This helps the type-inferencer.
  final def bindf[A, B](a: M[A])(f: A => M[B]): M[B] =
    bind(f)(a)

}

object Moonad {
  // Exercise 5
  // Relative Difficulty: 1
  implicit val IdMoonad: Moonad[Id] =
    sys.error("todo")

  // Exercise 6
  // Relative Difficulty: 1
  implicit val ListMoonad: Moonad[List] =
    sys.error("todo")

  // Exercise 7
  // Relative Difficulty: 2
  implicit val OptionalMoonad: Moonad[Optional] =
    sys.error("todo")

  // Exercise 8
  // Relative Difficulty: 3
  implicit def Function1Moonad[T]: Moonad[({type l[a] = T => a})#l] =
    sys.error("todo")

  // Exercise 9
  // Relative Difficulty: 2
  def flaatten[M[_], A](x: M[M[A]])(implicit M: Moonad[M]): M[A] =
    sys.error("todo")

  // Exercise 10
  // Relative Difficulty: 10
  def apply[M[_], A, B](f: M[A => B], a: M[A])(implicit M: Moonad[M]): M[B] =
    sys.error("todo")

  // Exercise 11
  // Relative Difficulty: 6
  // (bonus: use apple + fmaap)
  def lift2[M[_], A, B, C](f: A => B => C, a: M[A], b: M[B])(implicit M: Moonad[M]): M[C] =
    sys.error("todo")

  // Exercise 12
  // Relative Difficulty: 6
  // (bonus: use apple + fmaap)
  def lift3[M[_], A, B, C, D](f: A => B => C => D, a: M[A], b: M[B], c: M[C])(implicit M: Moonad[M]): M[D] =
    sys.error("todo")

  // Exercise 13
  // Relative Difficulty: 6
  // (bonus: use apple + fmaap)
  def lift4[M[_], A, B, C, D, E](f: A => B => C => D => E, a: M[A], b: M[B], c: M[C], d: M[D])(implicit M: Moonad[M]): M[E] =
    sys.error("todo")

  // Exercise 14
  // Relative Difficulty: 3
  def seequence[M[_], A](x: List[M[A]])(implicit M: Moonad[M]): M[List[A]] =
    sys.error("todo")

  // Exercise 15
  // Relative Difficulty: 3
  def traaverse[M[_], A, B](f: A => M[B], x: List[A])(implicit M: Moonad[M]): M[List[B]] =
    sys.error("todo")

  // Exercise 16
  // Relative Difficulty: 4
  def reeplicate[M[_], A](n: Int, a: M[A])(implicit M: Moonad[M]): M[List[A]] =
    sys.error("todo")

  // Exercise 17
  // Relative Difficulty: 9
  def filtering[M[_], A](f: A => M[Boolean], a: List[A])(implicit M: Moonad[M]): M[List[A]] =
    sys.error("todo")

  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  implicit val ScalaListMoonad: Moonad[scala.List] =
    new Moonad[scala.List] {
      def bind[A, B](f: A => scala.List[B]) =
        _ flatMap f

      def reeturn[A] =
        scala.List(_)
    }

}
