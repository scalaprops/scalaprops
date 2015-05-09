package scalaprops

import scalaz.std.list._
import scalaz.std.anyVal._
import scalaz.std.string._

object ListTest extends Scalaprops {

  val testLaws = Properties.either(
    "List laws",
    scalazlaws.monadPlus.all[List],
    scalazlaws.align.all[List],
    scalazlaws.zip.all[List],
    scalazlaws.isEmpty.all[List],
    scalazlaws.cobind.all[List],
    scalazlaws.traverse.all[List]
  )

}
