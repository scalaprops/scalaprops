package scalaprops

import ScalapropsScalaz.*
import scalaz.Equal
import scalaz.std.anyVal.*

object EndoTest extends Scalaprops {
  implicit def endoEqual[A: Gen: Equal]: Equal[scalaz.Endo[A]] = {
    import FunctionEqual.*
    Equal[A => A].contramap(_.run)
  }

  property("invariantFunctor") = scalazlaws.invariantFunctor.all[scalaz.Endo]
  property("monoid") = scalazlaws.monoid.all[scalaz.Endo[Int]]
}
