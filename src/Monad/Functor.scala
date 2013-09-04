package Monad

import Intro._
import Structure._

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]

  // Provided as a synonym for `fmap` but with the arguments flipped.
  // This helps the type-inferencer.
  final def apply[A, B](a: F[A])(f: A => B): F[B] =
    fmap(f)(a)
}

object Functor {
  // Exercise 1
  // Relative Difficulty: 1
  implicit val IdFunctor: Functor[Id] =
    new Functor[Id] {
      def fmap[A, B](f: A => B) =
        sys.error("todo")
    }

  // Exercise 2
  // Relative Difficulty: 2
  implicit val ListFunctor: Functor[List] =
    new Functor[List] {
      def fmap[A, B](f: A => B) =
        sys.error("todo")
    }

  // Exercise 3
  // Relative Difficulty: 2
  implicit val OptionalFunctor: Functor[Optional] =
    new Functor[Optional] {
      def fmap[A, B](f: A => B) =
        sys.error("todo")
    }

  // Exercise 4
  // Relative Difficulty: 3
  implicit def Function1Functor[T]: Functor[({type l[a] = T => a})#l] =
    new Functor[({type l[a] = T => a})#l] {
      def fmap[A, B](f: A => B) =
        sys.error("todo")
    }

  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  implicit val ScalaListFunctor: Functor[scala.List] =
    new Functor[scala.List] {
      def fmap[A, B](f: A => B) =
        _ map f
    }

}
