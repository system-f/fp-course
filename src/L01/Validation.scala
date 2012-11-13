package L01

import Validation._

sealed trait Validation[A] {
  def isError: Boolean =
    this match {
      case Error(_) => true
      case Value(_) => false
    }

  def isValue: Boolean =
    !isError

  def map[B](f: A => B): Validation[B] =
    this match {
      case Error(s) => Error(s)
      case Value(a) => Value(f(a))
    }

  def flatMap[B](f: A => Validation[B]): Validation[B] =
    this match {
      case Error(s) => Error(s)
      case Value(a) => f(a)
    }

  def valueOr(d: => A): A =
    this match {
      case Error(_) => d
      case Value(a) => a
    }

  def errorOr(a: => Err): Err =
    this match {
      case Error(e) => e
      case Value(_) => a
    }

}
case class Error[A](e: Err) extends Validation[A]
case class Value[A](a: A) extends Validation[A]

object Validation {
  type Err =
    String
}
