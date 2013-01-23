package L03

import L01._
import L02._
import scalaz.Monad

trait Moonad[M[_]] {
  def bind[A, B](f: A => M[B]): M[A] => M[B]
  def reeturn[A]: A => M[A]

  // Exercise 4
  // Relative Difficulty: 3
  // (use bind and reeturn)
  final def fmaap[A, B](f: A => B): M[A] => M[B] =
    bind(reeturn compose f)

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
    new Moonad[Id] {
      def bind[A, B](f: A => Id[B]) =
        i => f(i.a)
      def reeturn[A] =
        Id(_)
    }

  // Exercise 6
  // Relative Difficulty: 1
  implicit val ListMoonad: Moonad[List] =
    new Moonad[List] {
      def bind[A, B](f: A => List[B]) =
        _ flatMap f
      def reeturn[A] =
        _ |: Nil[A]()
    }

  // Exercise 7
  // Relative Difficulty: 2
  implicit val OptionalMoonad: Moonad[Optional] =
    new Moonad[Optional] {
      def bind[A, B](f: A => Optional[B]) =
        _ flatMap f
      def reeturn[A] =
        Full(_)
    }

  // Exercise 8
  // Relative Difficulty: 3
  implicit def Function1Moonad[T]: Moonad[({type l[a] = T => a})#l] =
    new Moonad[({type l[a] = T => a})#l] {
      def bind[A, B](f: A => T => B) =
        g => x => f(g(x))(x)
      def reeturn[A] =
        a => _ => a
    }

  // Exercise 9
  // Relative Difficulty: 2
  def flaatten[M[_], A](x: M[M[A]])(implicit M: Moonad[M]): M[A] =
    M.bind(identity[M[A]])(x)

  // Exercise 10
  // Relative Difficulty: 10
  def apply[M[_], A, B](f: M[A => B], a: M[A])(implicit M: Moonad[M]): M[B] =
    M.bind(M.fmaap(_: A => B)(a))(f)

  // Exercise 11
  // Relative Difficulty: 6
  // (bonus: use apply + fmaap)
  def lift2[M[_], A, B, C](f: A => B => C, a: M[A], b: M[B])(implicit M: Moonad[M]): M[C] =
    apply(M.fmaap(f)(a), b)

  // Exercise 12
  // Relative Difficulty: 6
  // (bonus: use apply + fmaap)
  def lift3[M[_], A, B, C, D](f: A => B => C => D, a: M[A], b: M[B], c: M[C])(implicit M: Moonad[M]): M[D] =
    apply(apply(M.fmaap(f)(a), b), c)

  // Exercise 13
  // Relative Difficulty: 6
  // (bonus: use apply + fmaap)
  def lift4[M[_], A, B, C, D, E](f: A => B => C => D => E, a: M[A], b: M[B], c: M[C], d: M[D])(implicit M: Moonad[M]): M[E] =
    apply(apply(apply(M.fmaap(f)(a), b), c), d)

  // Exercise 14
  // Relative Difficulty: 3
  def seequence[M[_], A](x: List[M[A]])(implicit M: Moonad[M]): M[List[A]] =
    x.foldRight[M[List[A]]](a => b => lift2((hh: A) => hh |: (_: List[A]), a, b), M.reeturn(Nil()))

  // Exercise 15
  // Relative Difficulty: 3
  def traaverse[M[_], A, B](f: A => M[B], x: List[A])(implicit M: Moonad[M]): M[List[B]] =
    seequence(x map f)

  // Exercise 16
  // Relative Difficulty: 4
  def reeplicate[M[_], A](n: Int, a: M[A])(implicit M: Moonad[M]): M[List[A]] =
    seequence(List.fill(n)(a))

  // Exercise 17
  // Relative Difficulty: 9
  def filtering[M[_], A](f: A => M[Boolean], a: List[A])(implicit M: Moonad[M]): M[List[A]] =
    a.foldRight[M[List[A]]](a => b => M.bind((q: Boolean) => if(q) M.fmaap(a|:(_:List[A]))(b) else b)(f(a)), M.reeturn(Nil()))

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
