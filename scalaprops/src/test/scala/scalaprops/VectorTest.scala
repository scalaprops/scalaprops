package scalaprops

import scalaz.std.vector._
import scalaz.std.anyVal._

object VectorTest extends Scalaprops {

  val testLaws =
    Properties.list(
      scalazlaws.monadPlusStrong.all[Vector],
      scalazlaws.traverse.all[Vector],
      scalazlaws.zip.all[Vector],
      scalazlaws.isEmpty.all[Vector],
      scalazlaws.align.all[Vector]
    )

}
