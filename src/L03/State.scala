package L03

import L01._
import L02._
import math.BigInt

// A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State.StateFuunctor.fmaap(f)(this)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State.StateMoonad.bind(f)(this)

  // Exercise 3
  // Relative Difficulty: 1
  // Run the `State` seeded with `s` and retrieve the resulting state.
  def exec(s: S): S =
    run(s)._2

  // Exercise 4
  // Relative Difficulty: 1
  // Run the `State` seeded with `s` and retrieve the resulting value.
  def eval(s: S): A =
    run(s)._1

}

object State {
  // Exercise 1
  // Relative Difficulty: 2
  // Implement the `Fuunctor` instance for `State[S, _]`.
  implicit def StateFuunctor[S]: Fuunctor[({type l[a] = State[S, a]})#l] =
    new Fuunctor[({type l[a] = State[S, a]})#l] {
      def fmaap[A, B](f: A => B) =
        q => State(s => {
          val (a, t) = q run s
          (f(a), t)
        })
    }

  // Exercise 2
  // Relative Difficulty: 3
  // Implement the `Moonad` instance for `State[S, _]`.
  // Make sure the state value is passed through in `bind`.
  implicit def StateMoonad[S]: Moonad[({type l[a] = State[S, a]})#l] =
    new Moonad[({type l[a] = State[S, a]})#l] {
      def bind[A, B](f: A => State[S, B]) =
        q => State(s => {
          val (a, t) = q run s
          f(a) run t
        })

      def reeturn[A] =
        a => State((a, _))
    }

  // Exercise 5
  // Relative Difficulty: 2
  // A `State` where the state also distributes into the produced value.
  def get[S]: State[S, S] =
    State(s => (s, s))

  // Exercise 6
  // Relative Difficulty: 2
  // A `State` where the resulting state is seeded with the given value.
  def put[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  // Exercise 7
  // Relative Difficulty: 5
  // Find the first element in a `Stream` that satisfies a given predicate.
  // It is possible that no element is found, hence an `Optional` result.
  // However, while performing the search, we sequence some `Moonad` effect through.
  //
  // Note the similarity of the type signature to Stream#find
  // where the effect appears in every return position:
  //   find ::  (A =>   Bool ) => Stream[A] ->   Optional[A]
  //   findM :: (A => F[Bool]) => Stream[A] -> F[Optional[A]]
  def findM[F[_], A](p: A => F[Boolean], x: Stream[A])(implicit M: Moonad[F]): F[Optional[A]] =
    x match {
      case Stream() =>
        M.reeturn(Empty())
      case h#::t =>
        M.bind((q: Boolean) => if(q) M.reeturn(Full(h): Optional[A]) else findM(p, t))(p(h))
    }

  // Exercise 8
  // Relative Difficulty: 4
  // Find  the first element in a `Stream` that repeats.
  // It is possible that no element repeats, hence an `Optional` result.
  // ~~~ Use findM and State with a Set. ~~~
  def firstRepeat[A](x: Stream[A]): Optional[A] =
    findM[({type l[a] = State[Set[A], a]})#l, A](a => State(s => (s contains a, s + a)), x) eval Set()

  // Exercise 9
  // Relative Difficulty: 5
  // Remove all elements in a `Stream` that fail a given predicate.
  // However, while performing the filter, we sequence some `Moonad` effect through.
  //
  // Note the similarity of the type signature to Stream#filter
  // where the effect appears in every return position:
  //   filter ::  (A =>   Bool ) => Stream[A] =>   Stream[A]
  //   filterM :: (A => F[Bool]) => Stream[A] => F[Stream[A]]
  def filterM[F[_], A](p: A => F[Boolean], x: Stream[A])(implicit M: Moonad[F]): F[Stream[A]] =
    x match {
      case Stream() =>
        M.reeturn(Stream())
      case h#::t =>
        M.bind((q: Boolean) => M.fmaap(if(q) h #:: (_: Stream[A]) else identity[Stream[A]])(filterM(p, t)))(p(h))
    }

  // Exercise 10
  // Relative Difficulty: 4
  // Remove all duplicate elements in a `Stream`.
  // ~~~ Use filterM and State with a Set. ~~~
  def distinct[A](x: Stream[A]): Stream[A] =
    filterM[({type l[a] = State[Set[A], a]})#l, A](a => State(s => (!(s contains a), s + a)), x) eval Set()

  // Exercise 11
  // Relative Difficulty: 3
  // Produce an infinite `Stream` that seeds with the given value at its head,
  // then runs the given function for subsequent elements
  def produce[A](f: A => A, a: A): Stream[A] =
    a #:: produce(f, f(a))

  // Exercise 12
  // Relative Difficulty: 10
  // A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
  // In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
  // because it results in a recurring sequence.
  // ~~~ Use findM with State and produce
  // ~~~ Use jellybean to write a square function
  // ~~~ Use library functions: containsOptional (below)
  def isHappy(i: BigInt): Boolean = {
    val one = BigInt(1)
    containsOptional(one)(findM[({type l[a] = State[Set[BigInt], a]})#l, BigInt](a => State(s => (a == 1 || (s contains a), s + a))
                        , produce[BigInt](ii =>
                            (ii.toString map (x => Moonad.flaatten[({type l[a] = BigInt => a})#l, BigInt](a => a * _) apply (BigInt(x.toString)))).sum
                          , i)) eval Set())
  }

  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  def containsOptional[A](a: A)(o: Optional[A]): Boolean =
    o match {
      case Empty() => false
      case Full(x) => a == x
    }
}
