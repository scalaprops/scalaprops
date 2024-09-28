package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object IndSeqTest extends Scalaprops {
  val testLaw =
    Properties.list(
      scalazlaws.monadPlusStrong.all[IndSeq],
      scalazlaws.traverse.all[IndSeq],
      scalazlaws.isEmpty.all[IndSeq]
    )
}
