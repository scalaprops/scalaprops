package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.stream.*

object StreamTest extends Scalaprops {
  property("bindRec") = scalazlaws.bindRec.laws[Stream].andThenParam(Param.maxSize(1))

  property("laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[Stream],
    scalazlaws.align.all[Stream],
    scalazlaws.zip.all[Stream],
    scalazlaws.isEmpty.all[Stream],
    scalazlaws.cobind.all[Stream],
    scalazlaws.traverse.all[Stream]
  )
}
