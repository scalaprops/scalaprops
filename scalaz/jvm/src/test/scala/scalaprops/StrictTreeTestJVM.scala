package scalaprops

import scalaz.std.anyVal.*

object StrictTreeTestJVM extends Scalaprops {
  val catalanNumber = TreeTest.sizeTest(
    List(1, 1, 2, 5, 14, 42),
    (i, seed) => ScalapropsScalaz.strictTreeGenSized[Unit](i).infiniteStream(seed = seed)
  )
}
