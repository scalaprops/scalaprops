package scalaprops

import ScalapropsScalaz.*
import scalaz.Diev
import scalaz.std.anyVal.*

object DievTest extends Scalaprops {
  property("law") = Properties.list(
    scalazlaws.equal.all[Diev[Int]],
    scalazlaws.monoid.all[Diev[Int]]
  )
}
