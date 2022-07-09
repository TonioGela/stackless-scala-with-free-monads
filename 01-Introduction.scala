//> using scala "2.13.8"
//> using plugin "org.typelevel:::kind-projector:0.13.2"
//> using plugin "com.olegpy::better-monadic-for:0.3.1"

// Type parameters order is the opposite of the tuple
case class State[S, +A](runS: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State[S, B] { s =>
    val (a, s1): (A, S) = runS(s)
    (f(a), s1): (B, S)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] { s =>
    val (a, s1): (A, S) = runS(s)
    val sb: State[S, B] = f(a)
    sb.runS(s1): (B, S)
  }
}

object State {

  def getState[S]: State[S, S]           = State(s => (s, s))
  def setState[S](s: S): State[S, Unit]  = State(_ => ((), s))
  def pureState[S, A](a: A): State[S, A] = State(s => (a, s))

  // Despite using foldLeft, we create a sequence of State.flatmap, 3 for
  // each fold, that may exceed the stack size and throw a StackOverflow
  def zipIndex[A](as: List[A]): List[(Int, A)] = {
    val empty: State[Int, List[(Int, A)]]  = pureState[Int, List[(Int, A)]](List.empty[(Int, A)])
    val result: State[Int, List[(Int, A)]] = as.foldLeft(empty)((acc: State[Int, List[(Int, A)]], a: A) =>
      for {
        xs: List[(Int, A)] <- acc
        n: Int             <- getState[Int]
        _: Unit            <- setState[Int](n + 1)
      } yield (n, a) :: xs
    )
    val (list: List[(Int, A)], state: Int) = result.runS(0) // launch the missiles
    list.reverse // We reverse since we prepended with :: before
  }
}
