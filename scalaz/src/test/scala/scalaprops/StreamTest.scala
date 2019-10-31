package scalaprops

import scalaz.std.stream._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object StreamTest extends Scalaprops {
  val bindRec = scalazlaws.bindRec.laws[Stream].andThenParam(Param.maxSize(1))

  val laws = Properties.list(
    scalazlaws.monadPlusStrong.all[Stream],
    scalazlaws.align.all[Stream],
    scalazlaws.zip.all[Stream],
    scalazlaws.isEmpty.all[Stream],
    scalazlaws.cobind.all[Stream],
    scalazlaws.traverse.all[Stream]
  )
}
