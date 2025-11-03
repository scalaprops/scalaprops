package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

@scalajs.js.annotation.JSExportAll
object Tuple6Test extends Scalaprops {
  type F[A] = Tuple6[IList[Boolean], IList[Byte], IList[Short], Short, Byte, A]

  val bindRec = scalazlaws.bindRec.all[({ type l[a] = (Byte, Byte, Byte, Byte, Byte, a) })#l]

  val laws0 = Properties.list(
    scalazlaws.monoid.all[F[Byte]],
    scalazlaws.order.all[F[Byte]]
  )

  val laws1 = Properties.list(
    scalazlaws.monad.all[F],
    scalazlaws.traverse.all[F]
  )
}
