package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

@scalajs.js.annotation.JSExportAll
object Tuple2Test extends Scalaprops {
  val bitraverse = scalazlaws.bitraverse.all[Tuple2]
  val associative = scalazlaws.associative.all[Tuple2]

  val bindRec = scalazlaws.bindRec.laws[({ type l[a] = (Byte, a) })#l]

  val laws1 = {
    type F[A] = (IList[Boolean], A)
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val laws0 = Properties.list(
    scalazlaws.monoid.all[(IList[Boolean], IList[Boolean])],
    scalazlaws.order.all[(Byte, Byte)]
  )
}
