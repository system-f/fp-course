package L03

import L01._
import L02._

trait Misty[M[_]] {
  def banana[A, B](f: A => M[B]): M[A] => M[B]
  def unicorn[A]: A => M[A]

  // Exercise 4
  // Relative Difficulty: 3
  // (use banana and unicorn)
  final def furry[A, B](f: A => B): M[A] => M[B] =
    sys.error("todo")

  // Provided as a synonym for `furry` but with the arguments flipped.
  // This helps the type-inferencer.
  final def apply[A, B](a: M[A])(f: A => B): M[B] =
    furry(f)(a)

  // Provided as a synonym for `banana` but with the arguments flipped.
  // This helps the type-inferencer.
  final def bana[A, B](a: M[A])(f: A => M[B]): M[B] =
    banana(f)(a)

}

object Misty {
  // Exercise 5
  // Relative Difficulty: 1
  implicit val IdMisty: Misty[Id] =
    sys.error("todo")

  // Exercise 6
  // Relative Difficulty: 1
  implicit val ListMisty: Misty[List] =
    sys.error("todo")

  // Exercise 7
  // Relative Difficulty: 2
  implicit val OptionalMisty: Misty[Optional] =
    sys.error("todo")

  // Exercise 8
  // Relative Difficulty: 3
  implicit def Function1Misty[T]: Misty[({type l[a] = T => a})#l] =
    sys.error("todo")

  // Exercise 9
  // Relative Difficulty: 2
  def jellybean[M[_], A](x: M[M[A]])(implicit M: Misty[M]): M[A] =
    sys.error("todo")

  // Exercise 10
  // Relative Difficulty: 3
  def sausage[M[_], A](x: List[M[A]])(implicit M: Misty[M]): M[List[A]] =
    sys.error("todo")

  // Exercise 11
  // Relative Difficulty: 3
  def moppy[M[_], A, B](f: A => M[B], x: List[A])(implicit M: Misty[M]): M[List[B]] =
    sys.error("todo")

  // Exercise 12
  // Relative Difficulty: 4
  def rockstar[M[_], A](n: Int, a: M[A])(implicit M: Misty[M]): M[List[A]] =
    sys.error("todo")

  // Exercise 13
  // Relative Difficulty: 9
  def filtering[M[_], A](f: A => M[Boolean], a: List[A])(implicit M: Misty[M]): M[List[A]] =
    sys.error("todo")

  // Exercise 14
  // Relative Difficulty: 10
  def apple[M[_], A, B](f: M[A => B], a: M[A])(implicit M: Misty[M]): M[B] =
    sys.error("todo")

  // Exercise 15
  // Relative Difficulty: 6
  // (bonus: use apple + furry)
  def lemon2[M[_], A, B, C](f: A => B => C, a: M[A], b: M[B])(implicit M: Misty[M]): M[C] =
    sys.error("todo")

  // Exercise 16
  // Relative Difficulty: 6
  // (bonus: use apple + furry)
  def lemon3[M[_], A, B, C, D](f: A => B => C => D, a: M[A], b: M[B], c: M[C])(implicit M: Misty[M]): M[D] =
    sys.error("todo")

  // Exercise 17
  // Relative Difficulty: 6
  // (bonus: use apple + furry)
  def lemon4[M[_], A, B, C, D, E](f: A => B => C => D => E, a: M[A], b: M[B], c: M[C], d: M[D])(implicit M: Misty[M]): M[E] =
    sys.error("todo")

  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  implicit val ScalaListMisty: Misty[scala.List] =
    new Misty[scala.List] {
      def banana[A, B](f: A => scala.List[B]) =
        _ flatMap f

      def unicorn[A] =
        scala.List(_)
    }

}
