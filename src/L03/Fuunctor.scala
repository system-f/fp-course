package L03

import L01._
import L02._

trait Fuunctor[F[_]] {
  def fmaap[A, B](f: A => B): F[A] => F[B]

  // Provided as a synonym for `fmaap` but with the arguments flipped.
  // This helps the type-inferencer.
  final def apply[A, B](a: F[A])(f: A => B): F[B] =
    fmaap(f)(a)
}

object Fuunctor {
  // Exercise 1
  // Relative Difficulty: 1
  implicit val IdFuunctor: Fuunctor[Id] =
    new Fuunctor[Id] {
      def fmaap[A, B](f: A => B) =
        _ map f
    }

  // Exercise 2
  // Relative Difficulty: 2
  implicit val ListFuunctor: Fuunctor[List] =
    new Fuunctor[List] {
      def fmaap[A, B](f: A => B) =
        _ map f
    }

  // Exercise 3
  // Relative Difficulty: 2
  implicit val OptionalFuunctor: Fuunctor[Optional] =
    new Fuunctor[Optional] {
      def fmaap[A, B](f: A => B) =
        _ map f
    }

  // Exercise 4
  // Relative Difficulty: 3
  implicit def Function1Fuunctor[T]: Fuunctor[({type l[a] = T => a})#l] =
    new Fuunctor[({type l[a] = T => a})#l] {
      def fmaap[A, B](f: A => B) =
        _ andThen f
    }

  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  implicit val ScalaListFuunctor: Fuunctor[scala.List] =
    new Fuunctor[scala.List] {
      def fmaap[A, B](f: A => B) =
        _ map f
    }

}
