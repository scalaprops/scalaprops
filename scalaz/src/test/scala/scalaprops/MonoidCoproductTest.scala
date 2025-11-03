package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object MonoidCoproductTest extends Scalaprops {
  val testLaws =
    Properties.list(
      scalazlaws.equal.all[Int :+: Int],
      scalazlaws.monoid.all[Int :+: Int]
    )
}
