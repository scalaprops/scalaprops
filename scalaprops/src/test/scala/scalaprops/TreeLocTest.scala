package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object TreeLocTest extends Scalaprops{

  val laws = Properties.list(
    scalazlaws.order.all[TreeLoc[Byte]],
    scalazlaws.traverse1.all[TreeLoc],
    scalazlaws.comonad.all[TreeLoc]
  )

  val treeLocGenSized = {
    Property.forAllG(Gen.positiveByte, Gen[Long]){ (n, seed) =>
      val size = 5
      val a = ScalapropsScalaz.treeLocGenSized[Unit](n).samples(
        listSize = size, seed = seed
      ).map(Foldable[TreeLoc].length)

      a == List.fill(size)(n)
    }
  }.toProperties((), Param.minSuccessful(10))

}
