package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object CoyonedaTest extends Scalaprops {

  val testOrderMaybe = scalazlaws.order.all[Coyoneda[Maybe, Int]]
  val testOrderIList = scalazlaws.order.all[Coyoneda[IList, Int]]

  val testNel = {
    type F[A] = Coyoneda[NonEmptyList, A]

    Properties.either(
      "Coyoneda[NonEmptyList, _]",
      scalazlaws.traverse1.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val testMaybe = {
    type F[A] = Coyoneda[Maybe, A]

    Properties.either(
      "Coyoneda[Maybe, _]",
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F],
      scalazlaws.cobind.all[F]
    )
  }

  val testIList = {
    type F[A] = Coyoneda[IList, A]

    Properties.either(
      "Coyoneda[IList, _]",
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F],
      scalazlaws.cobind.all[F]
    )
  }
}
