package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

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
