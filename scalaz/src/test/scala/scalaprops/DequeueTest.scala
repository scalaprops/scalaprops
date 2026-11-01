package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object DequeueTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.traverse.all[Dequeue],
    scalazlaws.monadPlus.all[Dequeue],
    scalazlaws.isEmpty.all[Dequeue]
  )
}
