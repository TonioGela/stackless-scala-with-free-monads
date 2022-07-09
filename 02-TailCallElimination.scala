//> using scala "2.13.8"
//> using plugin "org.typelevel:::kind-projector:0.13.2"
//> using plugin "com.olegpy::better-monadic-for:0.3.1"
import scala.annotation.tailrec

object TailCallElimination {

  // This is an tail rec optimizable function since the call to
  // itself happens in tail position (it has to be private or final
  // to avoid being overridden)
  @tailrec
  def foldl[A, B](as: List[A], b: B, f: (B, A) => B): B = as match {
    case Nil     => b
    case x :: xs => foldl(xs, f(b, x), f)
  }

  // The recursive method is replaced with a single jump in the
  // compiled code, that is equivalent to rewriting the method like
  def mutFoldl[A, B](as: List[A], b: B, f: (B, A) => B): B = {
    var z: B        = b
    var az: List[A] = as
    while (true) az match {
      case Nil     => return z
      case x :: xs =>
        z = f(z, x)
        az = xs
    }
    z
  }
  // O_O it took me a while to grasp this mutable code

  // A jump is faster than a method invocation + it doesn't
  // require space on the stack. Unfortunately replacing tail
  // calls with jump in general is difficult. Plus the JVM atm
  // doesn't support jumps to other methods, so mutual recursion
  // is not optimizable

  def even[A]: List[A] => Boolean = {
    case Nil     => true
    case _ :: xs => odd(xs)
  }

  def odd[A]: List[A] => Boolean = {
    case Nil     => false
    case _ :: xs => even(xs)
  }

}
