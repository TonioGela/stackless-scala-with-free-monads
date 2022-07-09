//> using scala "2.13.8"
//> using plugin "org.typelevel:::kind-projector:0.13.2"
//> using plugin "com.olegpy::better-monadic-for:0.3.1"

// Let's rewrite State with Trampoline

case class State[S, +A](runS: S => Trampoline[(A, S)]) {

  def flatMap[B](f: A => State[S, B]) = State[S, B](s =>
    More { () =>
      val t: Trampoline[(A, S)]  = runS(s)
      val (a, s1): (A, S)        = t.runT // The call to runT is not in tail position
      val fa: State[S, B]        = f(a)
      val tb: Trampoline[(B, S)] = fa.runS(s1)
      More(() => tb)
    }
  )

  // The call to runT is not in tail position though, so it cannot be optimized
}

// Let's try to solve the problem making Trampoline a monad so that we can
// write the State's flatMap like this:

case class AltState[S, +A](runS: S => Trampoline[(A, S)]) {

  def flatMap[B](f: A => AltState[S, B]) =
    AltState[S, B](s => More(() => runS(s).flatMap { case (a, s1) => More(() => f(a).runS(s1)) }))
}

// here's the monadic implementation
sealed trait Trampoline[+A] {
  import scala.annotation.tailrec

  @tailrec
  final def runT: A = this match {
    case Done(result) => result
    case More(k)      => k().runT
  }

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = More[B](() => f(runT))
  // This is not solving as well, since runT is still not in a tail position either
}

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
case class Done[+A](result: A)              extends Trampoline[A]
