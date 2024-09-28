package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object ApTest extends Scalaprops {
  val lawsMaybe = Properties.list(
    scalazlaws.equal.all[Ap[Maybe, Int]],
    scalazlaws.monoid.all[Ap[Maybe, Int]]
  )

  val lawsIList = Properties.list(
    scalazlaws.equal.all[Ap[IList, Int]],
    scalazlaws.monoid.all[Ap[IList, Int]]
  )
}
