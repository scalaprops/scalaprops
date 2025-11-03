package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

@scalajs.js.annotation.JSExportAll
object Tuple8Test extends Scalaprops {
  type F[A] = Tuple8[IList[Boolean], IList[Byte], IList[Short], Int, Short, Byte, Byte, A]

  val bindRec = scalazlaws.bindRec.all[({ type l[a] = (Byte, Byte, Byte, Byte, Byte, Byte, Byte, a) })#l]

  val laws0 = Properties.list(
    scalazlaws.monoid.all[F[Byte]],
    scalazlaws.order.all[F[Byte]]
  )

  val laws1 = Properties.list(
    scalazlaws.monad.all[F],
    scalazlaws.traverse.all[F]
  )
}
