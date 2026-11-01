package scalaprops

import scalaz.std.anyVal.*

object StrictTreeTestJVM extends Scalaprops {
  property("catalanNumber") = TreeTest.sizeTest(
    List(1, 1, 2, 5, 14, 42),
    (i, seed) => ScalapropsScalaz.strictTreeGenSized[Unit](i).infiniteStream(seed = seed)
  )
}
