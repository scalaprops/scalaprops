package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object MonoidCoproductTest extends Scalaprops {
  val testLaws =
    Properties.list(
      scalazlaws.equal.all[Int :+: Int],
      scalazlaws.monoid.all[Int :+: Int]
    )
}
