package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object MaybeTest extends Scalaprops {

  val testLaws = Properties.either(
    "Maybe laws",
    scalazlaws.monadPlus.all[Maybe],
    scalazlaws.traverse.all[Maybe],
    scalazlaws.zip.all[Maybe]
  )
}
