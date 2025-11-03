package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object TheseTest extends Scalaprops {
  private type F[A] = Int \&/ A

  val test0 =
    Properties.list(
      scalazlaws.order.all[Int \&/ Int],
      scalazlaws.band.all[ISet[Int] \&/ ISet[Int]]
    )

  // TODO bindRec.handleManyBinds
  val bindRec = scalazlaws.bindRec.tailrecBindConsistency[({ type l[a] = Byte \&/ a })#l, Byte]

  val test1 =
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.cobind.all[F],
      scalazlaws.traverse.all[F]
    )

  val test2 =
    scalazlaws.bitraverse.all[\&/]
}
