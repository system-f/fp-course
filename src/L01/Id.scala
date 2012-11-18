package L01

case class Id[A](a: A) {
  def map[B](f: A => B): Id[B] =
    Id(f(a))

  // aka bind
  def flatMap[B](f: A => Id[B]): Id[B] =
    f(a)
}