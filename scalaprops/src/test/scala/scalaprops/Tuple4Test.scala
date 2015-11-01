package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object Tuple4Test extends Scalaprops {

  type F[A] = Tuple4[IList[Boolean], IList[Byte], IList[Short], A]

  val bindRec = scalazlaws.bindRec.all[({type l[a] = (Byte, Byte, Byte, a)})#l]

  val laws0 = Properties.list(
    scalazlaws.monoid.all[F[Byte]],
    scalazlaws.order.all[F[Byte]]
  )

  val laws1 = Properties.list(
    scalazlaws.monad.all[F],
    scalazlaws.traverse.all[F]
  )

}
