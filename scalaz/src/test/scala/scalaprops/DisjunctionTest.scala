package scalaprops

import ScalapropsScalaz.*
import scalaz.IList
import scalaz.\/
import scalaz.std.anyVal.*

object DisjunctionTest extends Scalaprops {
  property("bitraverse") = scalazlaws.bitraverse.all[\/]
  property("associative") = scalazlaws.associative.all[\/]
  property("order") = scalazlaws.order.all[Int \/ Int]
  property("monoid") = scalazlaws.monoid.all[IList[Byte] \/ IList[Byte]]

  property("laws1") = Properties.list(
    scalazlaws.monadError.all[({ type l[a] = Int \/ a })#l, Int],
    scalazlaws.traverse.all[({ type l[a] = Int \/ a })#l],
    scalazlaws.bindRec.all[({ type l[a] = Int \/ a })#l],
    scalazlaws.plus.all[({ type l[a] = Int \/ a })#l]
  )
}
