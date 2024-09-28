package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object DequeueTest extends Scalaprops {
  val testLaws =
    Properties.list(
      scalazlaws.traverse.all[Dequeue],
      scalazlaws.monadPlus.all[Dequeue],
      scalazlaws.isEmpty.all[Dequeue]
    )
}
