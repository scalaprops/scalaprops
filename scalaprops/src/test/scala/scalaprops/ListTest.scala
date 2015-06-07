package scalaprops

import scalaz.std.list._
import scalaz.std.anyVal._

object ListTest extends Scalaprops {

  val testLaws = Properties.list(
    scalazlaws.monadPlusStrong.all[List],
    scalazlaws.align.all[List],
    scalazlaws.zip.all[List],
    scalazlaws.isEmpty.all[List],
    scalazlaws.cobind.all[List],
    scalazlaws.traverse.all[List]
  )

}
