package scalaprops

import scalaz.std.anyVal._

object TreeLocTestJVM extends Scalaprops {

  /**
   * @see [[https://oeis.org/A001791]]
   */
  val A001791 = TreeTest.sizeTest(
    List(1, 4, 15, 56),
    (i, seed) => Gen.treeLocGenSized[Unit](i).infiniteStream(seed = seed)
  )
}
