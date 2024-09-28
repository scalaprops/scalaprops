package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object ConstTest extends Scalaprops {
  val testInt = {
    type F[A] = Const[Int, A]
    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.contravariant.all[F],
      scalazlaws.monoid.all[Const[Byte, Byte]],
      scalazlaws.order.all[Const[Int, Int]]
    )
  }
}
