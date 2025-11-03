package scalaprops

import ScalapropsScalaz.*
import scalaz.IList
import scalaz.\/
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object DisjunctionTest extends Scalaprops {
  val bitraverse = scalazlaws.bitraverse.all[\/]
  val associative = scalazlaws.associative.all[\/]
  val order = scalazlaws.order.all[Int \/ Int]
  val monoid = scalazlaws.monoid.all[IList[Byte] \/ IList[Byte]]

  val laws1 = Properties.list(
    scalazlaws.monadError.all[({ type l[a] = Int \/ a })#l, Int],
    scalazlaws.traverse.all[({ type l[a] = Int \/ a })#l],
    scalazlaws.bindRec.all[({ type l[a] = Int \/ a })#l],
    scalazlaws.plus.all[({ type l[a] = Int \/ a })#l]
  )
}
