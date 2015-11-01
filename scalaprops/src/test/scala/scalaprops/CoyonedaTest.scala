package scalaprops

import scalaz._
import scalaz.std.anyVal._

object CoyonedaTest extends Scalaprops {

  val testOrderMaybe = scalazlaws.order.all[Coyoneda[Maybe, Int]]
  val testOrderIList = scalazlaws.order.all[Coyoneda[IList, Int]]

  val testNel = {
    type F[A] = Coyoneda[NonEmptyList, A]

    Properties.list(
      scalazlaws.traverse1.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.laws[F].andThenParam(Param.maxSize(1)),
      scalazlaws.plus.all[F]
    )
  }

  val testMaybe = {
    type F[A] = Coyoneda[Maybe, A]

    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.cobind.all[F]
    )
  }

  val testIList = {
    type F[A] = Coyoneda[IList, A]

    Properties.list(
      scalazlaws.bindRec.laws[F].andThenParam(Param.maxSize(1)),
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.cobind.all[F]
    )
  }
}
