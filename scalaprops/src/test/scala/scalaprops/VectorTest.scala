package scalaprops

import scalaz.std.vector._
import scalaz.std.anyVal._
import scalaz.std.string._

object VectorTest extends Scalaprops {

  val testLaws =
    Properties.either(
      "Vector",
      scalazlaws.monadPlus.all[Vector],
      scalazlaws.traverse.all[Vector],
      scalazlaws.zip.all[Vector],
      scalazlaws.isEmpty.all[Vector],
      scalazlaws.align.all[Vector]
    )

}
