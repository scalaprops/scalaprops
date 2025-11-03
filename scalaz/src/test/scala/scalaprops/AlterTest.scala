package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object AlterTest extends Scalaprops {
  val lawsMaybe = Properties.list(
    scalazlaws.equal.all[Alter[Maybe, Int]],
    scalazlaws.monoid.all[Alter[Maybe, Int]]
  )

  val lawsIList = Properties.list(
    scalazlaws.equal.all[Alter[IList, Int]],
    scalazlaws.monoid.all[Alter[IList, Int]]
  )
}
