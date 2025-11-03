package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

@scalajs.js.annotation.JSExportAll
object Tuple3Test extends Scalaprops {
  type F[A] = Tuple3[IList[Boolean], IList[Byte], A]

  val bindRec = scalazlaws.bindRec.all[({ type l[a] = (Byte, Byte, a) })#l]

  val laws0 = Properties.list(
    scalazlaws.monoid.all[F[Byte]],
    scalazlaws.order.all[F[Byte]]
  )

  val laws1 = Properties.list(
    scalazlaws.monad.all[F],
    scalazlaws.traverse.all[F]
  )
}
