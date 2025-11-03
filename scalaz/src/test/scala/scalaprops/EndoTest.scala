package scalaprops

import ScalapropsScalaz.*
import scalaz.Equal
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object EndoTest extends Scalaprops {
  implicit def endoEqual[A: Gen: Equal]: Equal[scalaz.Endo[A]] = {
    import FunctionEqual.*
    Equal[A => A].contramap(_.run)
  }

  val testInvariantFunctor = scalazlaws.invariantFunctor.all[scalaz.Endo]
  val testMonoid = scalazlaws.monoid.all[scalaz.Endo[Int]]
}
