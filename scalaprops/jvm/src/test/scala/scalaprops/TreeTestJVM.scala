package scalaprops

import scalaz.std.anyVal._

object TreeTestJVM extends Scalaprops {
  /**
    * @see [[https://en.wikipedia.org/wiki/Catalan_number]]
    */
  val catalanNumber = TreeTest.sizeTest(
    List(1, 1, 2, 5, 14, 42),
    (i, seed) => Gen.treeGenSized[Unit](i).infiniteStream(seed = seed)
  )
}
