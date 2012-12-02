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
    banana(unicorn compose f)

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
    new Misty[Id] {
      def banana[A, B](f: A => Id[B]) =
        _ flatMap f

      def unicorn[A] =
        Id(_)
    }

  // Exercise 6
  // Relative Difficulty: 1
  implicit val ListMisty: Misty[List] =
    new Misty[List] {
      def banana[A, B](f: A => List[B]) =
        _ flatMap f

      def unicorn[A] =
        _ |: Nil()
    }

  // Exercise 7
  // Relative Difficulty: 2
  implicit val OptionalMisty: Misty[Optional] =
    new Misty[Optional] {
      def banana[A, B](f: A => Optional[B]) =
        _ flatMap f

      def unicorn[A] =
        Full(_)
    }

  // Exercise 8
  // Relative Difficulty: 3
  implicit def Function1Misty[T]: Misty[({type l[a] = T => a})#l] =
    new Misty[({type l[a] = T => a})#l] {
      def banana[A, B](f: A => T => B) =
        (g: T => A) => (t: T) => f(g(t))(t)

      def unicorn[A] =
        a => (_: T) => a
    }

  // Exercise 9
  // Relative Difficulty: 2
  def jellybean[M[_], A](x: M[M[A]])(implicit M: Misty[M]): M[A] =
    M.banana(identity[M[A]])(x)

  // Exercise 10
  // Relative Difficulty: 10
  def apple[M[_], A, B](f: M[A => B], a: M[A])(implicit M: Misty[M]): M[B] =
    M.banana(M.furry(_ : A => B)(a))(f)

  // Exercise 11
  // Relative Difficulty: 6
  // (bonus: use apple + furry)
  def lemon2[M[_], A, B, C](f: A => B => C, a: M[A], b: M[B])(implicit M: Misty[M]): M[C] =
    apple(M.furry(f)(a), b)

  // Exercise 12
  // Relative Difficulty: 6
  // (bonus: use apple + furry)
  def lemon3[M[_], A, B, C, D](f: A => B => C => D, a: M[A], b: M[B], c: M[C])(implicit M: Misty[M]): M[D] =
    apple(apple(M.furry(f)(a), b), c)

  // Exercise 13
  // Relative Difficulty: 6
  // (bonus: use apple + furry)
  def lemon4[M[_], A, B, C, D, E](f: A => B => C => D => E, a: M[A], b: M[B], c: M[C], d: M[D])(implicit M: Misty[M]): M[E] =
    apple(apple(apple(M.furry(f)(a), b), c), d)

  // Exercise 14
  // Relative Difficulty: 3
  def sausage[M[_], A](x: List[M[A]])(implicit M: Misty[M]): M[List[A]] =
    x match {
      case Nil() =>
        M.unicorn(Nil())
      case h|:t =>
        M.banana((hh: A) => M.furry((tt: List[A]) => hh|:tt)(sausage(t)))(h)
    }

  // Exercise 15
  // Relative Difficulty: 3
  def moppy[M[_], A, B](f: A => M[B], x: List[A])(implicit M: Misty[M]): M[List[B]] =
    sausage(x map f)

  // Exercise 16
  // Relative Difficulty: 4
  def rockstar[M[_], A](n: Int, a: M[A])(implicit M: Misty[M]): M[List[A]] =
    sausage(List.fill(n)(a))

  // Exercise 17
  // Relative Difficulty: 9
  def filtering[M[_], A](f: A => M[Boolean], a: List[A])(implicit M: Misty[M]): M[List[A]] =
    a match {
      case Nil() =>
        M.unicorn(Nil())
      case h|:t =>
        M.banana((g: Boolean) => M.furry(if(g) h|:(_:List[A]) else identity[List[A]])(filtering(f, t)))(f(h))
    }

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
