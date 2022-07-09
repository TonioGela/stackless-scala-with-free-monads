//> using scala "2.13.8"
//> using plugin "org.typelevel:::kind-projector:0.13.2"
//> using plugin "com.olegpy::better-monadic-for:0.3.1"
import scala.annotation.tailrec

// This can be seen as a simpler brother of
// [scala.util.control.TailCalls.TailRec]
sealed trait Trampoline[+A] {

  // It's final so that the compiler can eliminate this tail call
  @tailrec
  final def runT: A = this match {
    case Done(result) => result
    case More(k)      => k().runT
  }
}

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
case class Done[+A](result: A)              extends Trampoline[A]

// We can now solve the mutual recursion problem saw before
object TrampolinedMutualRecursion {

  def even[A]: List[A] => Trampoline[Boolean] = {
    case Nil     => Done(true)
    case _ :: xs => More(() => odd(xs))
  }

  def odd[A]: List[A] => Trampoline[Boolean] = {
    case Nil     => Done(false)
    case _ :: xs => More(() => even(xs))
  }
}
// It is safe since it can be executed tail recursively calling the runT method
