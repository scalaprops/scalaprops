package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object OneAndTest extends Scalaprops {

  val testIList = {
    type F[A] = OneAnd[IList, A]

    Properties.either(
      "OneAnd[IList, A]",
      scalazlaws.order.all[F[Int]],
      scalazlaws.monad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F]
    )
  }

  val testMaybe = {
    type F[A] = OneAnd[Maybe, A]

    Properties.either(
      "OneAnd[Maybe, A]",
      scalazlaws.order.all[F[Int]],
      scalazlaws.monad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val testNel = {
    type F[A] = OneAnd[NonEmptyList, A]

    Properties.either(
      "OneAnd[NonEmptyList, A]",
      scalazlaws.order.all[F[Int]],
      scalazlaws.bind.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F],
      scalazlaws.plus.all[F]
    )
  }

}
