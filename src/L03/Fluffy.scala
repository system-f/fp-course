package L03

import L01._
import L02._

trait Fluffy[F[_]] {
  def furry[A, B](f: A => B): F[A] => F[B]

  // Provided as a synonym for `furry` but with the arguments flipped.
  // This helps the type-inferencer.
  final def apply[A, B](a: F[A])(f: A => B): F[B] =
    furry(f)(a)
}

object Fluffy {
  // Exercise 1
  // Relative Difficulty: 1
  implicit val IdFluffy: Fluffy[Id] =
    sys.error("todo")

  // Exercise 2
  // Relative Difficulty: 2
  implicit val ListFluffy: Fluffy[List] =
    sys.error("todo")

  // Exercise 3
  // Relative Difficulty: 2
  implicit val OptionalFluffy: Fluffy[Optional] =
    sys.error("todo")

  // Exercise 4
  // Relative Difficulty: 3
  implicit def Function1Fluffy[T]: Fluffy[({type l[a] = T => a})#l] =
    sys.error("todo")
  
  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  implicit val ScalaListFluffy: Fluffy[scala.List] =
    new Fluffy[scala.List] {
      def furry[A, B](f: A => B) =
        _ map f
    }

}
