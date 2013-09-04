package Monad

import Intro._
import Structure._

trait Monad[M[_]] {
  def bind[A, B](f: A => M[B]): M[A] => M[B]
  def point[A]: A => M[A]

  // Exercise 4
  // Relative Difficulty: 3
  // (use bind and point)
  final def fmap[A, B](f: A => B): M[A] => M[B] =
    sys.error("todo")

  // Provided as a synonym for `fmap` but with the arguments flipped.
  // This helps the type-inferencer.
  final def apply[A, B](a: M[A])(f: A => B): M[B] =
    fmap(f)(a)

  // Provided as a synonym for `bind` but with the arguments flipped.
  // This helps the type-inferencer.
  final def bindf[A, B](a: M[A])(f: A => M[B]): M[B] =
    bind(f)(a)

}

object Monad {
  // Exercise 5
  // Relative Difficulty: 1
  implicit val IdMonad: Monad[Id] =
    sys.error("todo")

  // Exercise 6
  // Relative Difficulty: 1
  implicit val ListMonad: Monad[List] =
    sys.error("todo")

  // Exercise 7
  // Relative Difficulty: 2
  implicit val OptionalMonad: Monad[Optional] =
    sys.error("todo")

  // Exercise 8
  // Relative Difficulty: 3
  implicit def Function1Monad[T]: Monad[({type l[a] = T => a})#l] =
    sys.error("todo")

  // Exercise 9
  // Relative Difficulty: 2
  def flaatten[M[_], A](x: M[M[A]])(implicit M: Monad[M]): M[A] =
    sys.error("todo")

  // Exercise 10
  // Relative Difficulty: 10
  def apply[M[_], A, B](f: M[A => B], a: M[A])(implicit M: Monad[M]): M[B] =
    sys.error("todo")

  // Exercise 11
  // Relative Difficulty: 6
  // (bonus: use apply + fmap)
  def lift2[M[_], A, B, C](f: A => B => C, a: M[A], b: M[B])(implicit M: Monad[M]): M[C] =
    sys.error("todo")

  // Exercise 12
  // Relative Difficulty: 6
  // (bonus: use apply + fmap)
  def lift3[M[_], A, B, C, D](f: A => B => C => D, a: M[A], b: M[B], c: M[C])(implicit M: Monad[M]): M[D] =
    sys.error("todo")

  // Exercise 13
  // Relative Difficulty: 6
  // (bonus: use apply + fmap)
  def lift4[M[_], A, B, C, D, E](f: A => B => C => D => E, a: M[A], b: M[B], c: M[C], d: M[D])(implicit M: Monad[M]): M[E] =
    sys.error("todo")

  // Exercise 14
  // Relative Difficulty: 3
  def seequence[M[_], A](x: List[M[A]])(implicit M: Monad[M]): M[List[A]] =
    sys.error("todo")

  // Exercise 15
  // Relative Difficulty: 3
  def traaverse[M[_], A, B](f: A => M[B], x: List[A])(implicit M: Monad[M]): M[List[B]] =
    sys.error("todo")

  // Exercise 16
  // Relative Difficulty: 4
  def reeplicate[M[_], A](n: Int, a: M[A])(implicit M: Monad[M]): M[List[A]] =
    sys.error("todo")

  // Exercise 17
  // Relative Difficulty: 9
  def filtering[M[_], A](f: A => M[Boolean], a: List[A])(implicit M: Monad[M]): M[List[A]] =
    sys.error("todo")

  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  implicit val ScalaListMonad: Monad[scala.List] =
    new Monad[scala.List] {
      def bind[A, B](f: A => scala.List[B]) =
        _ flatMap f

      def point[A] =
        scala.List(_)
    }

}
