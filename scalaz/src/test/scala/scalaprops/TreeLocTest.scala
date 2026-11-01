package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object TreeLocTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.order.all[TreeLoc[Byte]],
    scalazlaws.traverse1.all[TreeLoc],
    scalazlaws.comonad.all[TreeLoc]
  )

  property("treeLocGenSized") = {
    Property.forAllG(Gen.positiveByte, Gen[Long]) { (n, seed) =>
      val size = 5
      val a = ScalapropsScalaz
        .treeLocGenSized[Unit](n)
        .samples(
          listSize = size,
          seed = seed
        )
        .map(Foldable[TreeLoc].length)

      a == List.fill(size)(n)
    }
  }.toProperties((), Param.minSuccessful(10))
}
