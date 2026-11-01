package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object ApTest extends Scalaprops {
  property("lawsMaybe") = Properties.list(
    scalazlaws.equal.all[Ap[Maybe, Int]],
    scalazlaws.monoid.all[Ap[Maybe, Int]]
  )

  property("lawsIList") = Properties.list(
    scalazlaws.equal.all[Ap[IList, Int]],
    scalazlaws.monoid.all[Ap[IList, Int]]
  )
}
