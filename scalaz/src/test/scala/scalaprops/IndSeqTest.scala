package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object IndSeqTest extends Scalaprops {
  property("law") = Properties.list(
    scalazlaws.monadPlusStrong.all[IndSeq],
    scalazlaws.traverse.all[IndSeq],
    scalazlaws.isEmpty.all[IndSeq]
  )
}
