package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object MaybeTest extends Scalaprops {
  val testLaws = Properties.list(
    scalazlaws.monadPlusStrong.all[Maybe],
    scalazlaws.bindRec.all[Maybe],
    scalazlaws.traverse.all[Maybe],
    scalazlaws.isEmpty.all[Maybe],
    scalazlaws.cobind.all[Maybe],
    scalazlaws.align.all[Maybe],
    scalazlaws.zip.all[Maybe]
  )

  val test2 = Properties.list(
    scalazlaws.monoid.all[Maybe[Int]],
    scalazlaws.semilattice.all[Maybe[ISet[Int]]]
  )
}
