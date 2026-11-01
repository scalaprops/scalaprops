package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object TheseTest extends Scalaprops {
  private type F[A] = Int \&/ A

  property("test0") = Properties.list(
    scalazlaws.order.all[Int \&/ Int],
    scalazlaws.band.all[ISet[Int] \&/ ISet[Int]]
  )

  // TODO bindRec.handleManyBinds
  property("bindRec") = scalazlaws.bindRec.tailrecBindConsistency[({ type l[a] = Byte \&/ a })#l, Byte]

  property("test1") = Properties.list(
    scalazlaws.monad.all[F],
    scalazlaws.cobind.all[F],
    scalazlaws.traverse.all[F]
  )

  property("test2") = scalazlaws.bitraverse.all[\&/]
}
