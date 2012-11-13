package L01

sealed trait Optional[A] {
  def map[B](f: A => B): Optional[B] =
    this match {
      case Empty() => Empty()
      case Full(a) => Full(f(a))
    }

  // aka bind
  def flatMap[B](f: A => Optional[B]): Optional[B] =
    this match {
      case Empty() => Empty()
      case Full(a) => f(a)
    }

  def ??(d: => A): A =
    this match {
      case Empty() => d
      case Full(a) => a
    }

  def <+>(o: => Optional[A]): Optional[A] =
    this match {
      case Empty() => o
      case Full(_) => this
    }
}
case class Full[A](a: A) extends Optional[A]
case class Empty[A]() extends Optional[A]
