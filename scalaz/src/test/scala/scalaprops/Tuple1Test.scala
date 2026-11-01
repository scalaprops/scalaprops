package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

object Tuple1Test extends Scalaprops {
  property("laws0") = Properties.list(
    scalazlaws.order.all[Tuple1[Byte]],
    scalazlaws.monoid.all[Tuple1[Byte]]
  )

  property("laws1") = Properties.list(
    scalazlaws.traverse.all[Tuple1],
    scalazlaws.monad.all[Tuple1],
    scalazlaws.comonad.all[Tuple1]
  )
}
