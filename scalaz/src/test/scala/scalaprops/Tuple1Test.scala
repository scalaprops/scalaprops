package scalaprops

import scalaz.std.tuple._
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object Tuple1Test extends Scalaprops {
  val laws0 = Properties.list(
    scalazlaws.order.all[Tuple1[Byte]],
    scalazlaws.monoid.all[Tuple1[Byte]]
  )

  val laws1 = Properties.list(
    scalazlaws.traverse.all[Tuple1],
    scalazlaws.monad.all[Tuple1],
    scalazlaws.comonad.all[Tuple1]
  )
}
