package scalaprops

import ScalapropsScalaz.*
import scalaz.Diev
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object DievTest extends Scalaprops {
  val testLaw =
    Properties.list(
      scalazlaws.equal.all[Diev[Int]],
      scalazlaws.monoid.all[Diev[Int]]
    )
}
