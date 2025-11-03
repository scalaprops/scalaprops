package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object DequeueTest extends Scalaprops {
  val testLaws =
    Properties.list(
      scalazlaws.traverse.all[Dequeue],
      scalazlaws.monadPlus.all[Dequeue],
      scalazlaws.isEmpty.all[Dequeue]
    )
}
