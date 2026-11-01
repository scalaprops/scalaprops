package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object MaybeTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[Maybe],
    scalazlaws.bindRec.all[Maybe],
    scalazlaws.traverse.all[Maybe],
    scalazlaws.isEmpty.all[Maybe],
    scalazlaws.cobind.all[Maybe],
    scalazlaws.align.all[Maybe],
    scalazlaws.zip.all[Maybe]
  )

  property("test2") = Properties.list(
    scalazlaws.monoid.all[Maybe[Int]],
    scalazlaws.semilattice.all[Maybe[ISet[Int]]]
  )
}
