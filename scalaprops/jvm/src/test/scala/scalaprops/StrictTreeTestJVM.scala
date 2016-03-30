package scalaprops

import scalaz.std.anyVal._

object StrictTreeTestJVM extends Scalaprops {

  val catalanNumber = TreeTest.sizeTest(
    List(1, 1, 2, 5, 14, 42),
    (i, seed) => Gen.strictTreeGenSized[Unit](i).infiniteStream(seed = seed)
  )

}
