package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._
import ScalapropsScalaz._

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
