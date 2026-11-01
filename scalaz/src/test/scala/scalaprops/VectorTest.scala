package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.vector.*

object VectorTest extends Scalaprops {
  property("bindRec") = scalazlaws.bindRec.laws[Vector].andThenParam(Param.maxSize(1))

  property("laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[Vector],
    scalazlaws.traverse.all[Vector],
    scalazlaws.zip.all[Vector],
    scalazlaws.isEmpty.all[Vector],
    scalazlaws.align.all[Vector]
  )
}
