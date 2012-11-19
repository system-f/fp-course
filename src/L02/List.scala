package L02

// + Complete the 10 exercises below by filling out the function bodies.
//   Replace the function bodies (sys.error("todo")) with an appropriate solution.
// + These exercises may be done in any order, however:
//   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
// + Bonus for using the provided functions or for using one exercise solution to help solve another.
// + Approach with your best available intuition; just dive in and do what you can!

// TOTAL marks:    /66

sealed trait List[A] {
  final def foldRight[B](f: A => B => B, b: B): B =
    this match {
      case Nil() => b
      case h|:t => f(h)(t.foldRight(f, b))
    }

  @annotation.tailrec
  final def foldLeft[B](f: B => A => B, b: B): B =
    this match {
      case Nil() => b
      case h|:t => t.foldLeft(f, f(b)(h))
    }

  def |:(a: A): List[A] =
    new |:(a, this)

  // Exercise 1
  // Relative Difficulty: 1
  // Correctness: 2.0 marks
  // Performance: 0.5 mark
  // Elegance: 0.5 marks
  // Total: 3
  def headOr(a: => A): A =
    sys.error("todo")

  // Exercise 3
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def len: Int =
    sys.error("todo")

  // Exercise 4
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.0 mark
  // Elegance: 1.5 marks
  // Total: 7
  def map[B](f: A => B): List[B] =
    sys.error("todo")

  // Exercise 5
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def filter(f: A => Boolean): List[A] =
    sys.error("todo")

  // Exercise 6
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def append(x: List[A]): List[A] =
    sys.error("todo")

  // Exercise 8
  // Relative Difficulty: 7
  // Correctness: 5.0 marks
  // Performance: 1.5 marks
  // Elegance: 1.5 mark
  // Total: 8
  def flatMap[B](f: A => List[B]): List[B] =
    sys.error("todo")

  // Exercise 10
  // Relative Difficulty: 10
  // Correctness: 5.0 marks
  // Performance: 2.5 marks
  // Elegance: 2.5 marks
  // Total: 10
  def rev: List[A] =
    sys.error("todo")

  override def toString: String =
    foldRight[scala.List[A]](a => a :: _, scala.Nil).toString
}
case class Nil[A]() extends List[A]
case class |:[A](h: A, t: List[A]) extends List[A]

object List {
  // Exercise 2
  // Relative Difficulty: 2
  // Correctness:   2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def sum(x: List[Int]): Int =
    sys.error("todo")

  // Exercise 7
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def flatten[A](x: List[List[A]]): List[A] =
    sys.error("todo")

  // Exercise 9
  // Relative Difficulty: 8
  // Correctness: 3.5 marks
  // Performance: 2.0 marks
  // Elegance: 3.5 marks
  // Total: 9
  def seqf[A, B](x: List[A => B]): A => List[B] =
    sys.error("todo")

  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  def fill[A](n: Int)(a: A): List[A] =
    if(n <= 0)
      Nil()
    else
      a |: fill(n - 1)(a)

}
