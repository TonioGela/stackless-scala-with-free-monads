//> using scala "2.13.8"
//> using plugin "org.typelevel:::kind-projector:0.13.2"
//> using plugin "com.olegpy::better-monadic-for:0.3.1"

// One way to fix the tailcall problem might be to convert flatmap
// from a method call to a constructor call

case class More[+A](k: () => Trampoline[A])                          extends Trampoline[A]
case class Done[+A](result: A)                                       extends Trampoline[A]
case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]
// This can be thought as a call to a subroutine whose result is
// then fed to the continuation k

sealed trait Trampoline[+A] {
  import scala.annotation.tailrec

  // Now we should handle FlatMap too

  @tailrec
  final def runT: A = this match {
    case Done(result)    => result
    case More(k)         => k().runT
    case FlatMap(sub, k) => ???
  }
}
