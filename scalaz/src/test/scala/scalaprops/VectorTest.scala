package scalaprops

import scalaz.std.vector._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object VectorTest extends Scalaprops {

  val bindRec = scalazlaws.bindRec.laws[Vector].andThenParam(Param.maxSize(1))

  val testLaws =
    Properties.list(
      scalazlaws.monadPlusStrong.all[Vector],
      scalazlaws.traverse.all[Vector],
      scalazlaws.zip.all[Vector],
      scalazlaws.isEmpty.all[Vector],
      scalazlaws.align.all[Vector]
    )

}
