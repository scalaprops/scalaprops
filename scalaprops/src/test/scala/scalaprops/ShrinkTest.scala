package scalaprops

import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.stream._

object ShrinkTest extends Scalaprops {

  private[this] implicit def equal[A: Gen: Equal]: Equal[Shrink[A]] = {
    import FunctionEqual._
    Equal[A => Stream[A]].contramap(_.f)
  }

  val law = scalazlaws.invariantFunctor.all[Shrink]

}
