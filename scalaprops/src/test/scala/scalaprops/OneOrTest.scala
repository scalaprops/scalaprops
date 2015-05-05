package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object OneOrTest extends Scalaprops {

  val testIList = {
    type F[A] = OneOr[IList, A]

    Properties.either(
      "OneOr[IList, A]",
      scalazlaws.order.all[F[Int]],
      scalazlaws.cobind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val testNel = {
    type F[A] = OneOr[NonEmptyList, A]

    Properties.either(
      "OneOr[NonEmptyList, A]",
      scalazlaws.order.all[F[Int]],
      scalazlaws.traverse1.all[F],
      scalazlaws.comonad.all[F]
    )
  }

}
