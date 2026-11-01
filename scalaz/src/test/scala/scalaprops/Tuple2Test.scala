package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

object Tuple2Test extends Scalaprops {
  property("bitraverse") = scalazlaws.bitraverse.all[Tuple2]
  property("associative") = scalazlaws.associative.all[Tuple2]

  property("bindRec") = scalazlaws.bindRec.laws[({ type l[a] = (Byte, a) })#l]

  property("laws1") = {
    type F[A] = (IList[Boolean], A)
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  property("laws0") = Properties.list(
    scalazlaws.monoid.all[(IList[Boolean], IList[Boolean])],
    scalazlaws.order.all[(Byte, Byte)]
  )
}
