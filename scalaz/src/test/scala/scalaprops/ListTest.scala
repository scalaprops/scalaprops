package scalaprops

import scalaz.std.list._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object ListTest extends Scalaprops {

  val testLaws = Properties.list(
    scalazlaws.monadPlusStrong.all[List],
    scalazlaws.align.all[List],
    scalazlaws.zip.all[List],
    scalazlaws.isEmpty.all[List],
    scalazlaws.cobind.all[List],
    scalazlaws.traverse.all[List]
  )

  val bindRec = scalazlaws.bindRec.laws[List].andThenParam(Param.maxSize(1))

}
