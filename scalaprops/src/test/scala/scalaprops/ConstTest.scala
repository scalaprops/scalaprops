package scalaprops

import scalaz._
import scalaz.std.anyVal._

object ConstTest extends Scalaprops {

  val testInt = {
    type F[A] = Const[Int, A]
    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.contravariant.all[F],
      scalazlaws.order.all[Const[Int, Int]]
    )
  }

}
