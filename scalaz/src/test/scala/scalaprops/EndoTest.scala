package scalaprops

import scalaz.Equal
import scalaz.std.anyVal._
import ScalapropsScalaz._

object EndoTest extends Scalaprops {
  implicit def endoEqual[A: Gen: Equal]: Equal[scalaz.Endo[A]] = {
    import FunctionEqual._
    Equal[A => A].contramap(_.run)
  }

  val testInvariantFunctor = scalazlaws.invariantFunctor.all[scalaz.Endo]
  val testMonoid = scalazlaws.monoid.all[scalaz.Endo[Int]]
}
