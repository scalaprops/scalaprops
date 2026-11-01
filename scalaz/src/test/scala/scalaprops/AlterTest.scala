package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object AlterTest extends Scalaprops {
  property("lawsMaybe") = Properties.list(
    scalazlaws.equal.all[Alter[Maybe, Int]],
    scalazlaws.monoid.all[Alter[Maybe, Int]]
  )

  property("lawsIList") = Properties.list(
    scalazlaws.equal.all[Alter[IList, Int]],
    scalazlaws.monoid.all[Alter[IList, Int]]
  )
}
