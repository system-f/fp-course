package L03

import L01._
import L02._

trait Fluffy[F[_]] {
  def furry[A, B](f: A => B): F[A] => F[B]
}

object Fluffy {
  // Exercise 1
  // Relative Difficulty: 1
  implicit val ListFluffy: Fluffy[List] =
    new Fluffy[List] {
      def furry[A, B](f: A => B) =
        _ map f
    }

  // Exercise 2
  // Relative Difficulty: 1
  implicit val OptionalFluffy: Fluffy[Optional] =
    new Fluffy[Optional] {
      def furry[A, B](f: A => B) =
        _ map f
    }

  // Exercise 3
  // Relative Difficulty: 2
  implicit def Function1Fluffy[T]: Fluffy[({type l[a] = T => a})#l] =
    new Fluffy[({type l[a] = T => a})#l] {
      def furry[A, B](f: A => B) =
        _ andThen f
    }
  
  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  implicit val ScalaListFluffy: Fluffy[scala.List] =
    new Fluffy[scala.List] {
      def furry[A, B](f: A => B) =
        _ map f
    }

}
