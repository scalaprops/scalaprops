package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object Tuple3Test extends Scalaprops {

  type F[A] = Tuple3[IList[Boolean], IList[Byte], A]

  val laws0 = Properties.list(
    scalazlaws.monoid.all[F[Byte]],
    scalazlaws.order.all[F[Byte]]
  )

  val laws1 = Properties.list(
    scalazlaws.monad.all[F],
    scalazlaws.traverse.all[F]
  )

}
