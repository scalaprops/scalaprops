package scalaprops

import scalaz._
import scalaz.std.anyVal._

object TreeLocTest extends Scalaprops{

  val laws = Properties.list(
    scalazlaws.order.all[TreeLoc[Byte]],
    scalazlaws.traverse1.all[TreeLoc],
    scalazlaws.comonad.all[TreeLoc]
  )

  val treeLocGenSized = {
    Property.forAllG(Gen.positiveByte, Gen[Long]){ (n, seed) =>
      val size = 5
      val a = Gen.treeLocGenSized[Unit](n).samples(
        listSize = size, seed = seed
      ).map(Foldable[TreeLoc].length)

      a == List.fill(size)(n)
    }
  }

  /**
   * @see [[https://oeis.org/A001791]]
   */
  val A001791 = TreeTest.sizeTest(
    List(1, 4, 15, 56, 210),
    (i, seed) => Gen.treeLocGenSized[Unit](i).infiniteStream(seed = seed)
  )

}
