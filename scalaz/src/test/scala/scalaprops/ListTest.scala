package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.list.*

object ListTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[List],
    scalazlaws.align.all[List],
    scalazlaws.zip.all[List],
    scalazlaws.isEmpty.all[List],
    scalazlaws.cobind.all[List],
    scalazlaws.traverse.all[List]
  )

  property("bindRec") = scalazlaws.bindRec.laws[List].andThenParam(Param.maxSize(1))
}
