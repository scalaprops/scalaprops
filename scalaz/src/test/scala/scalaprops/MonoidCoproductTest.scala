package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object MonoidCoproductTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.equal.all[Int :+: Int],
    scalazlaws.monoid.all[Int :+: Int]
  )
}
