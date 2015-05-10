package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object CoproductTest extends Scalaprops {

  val testCoproductNelNel = {
    type F[A] = Coproduct[NonEmptyList, NonEmptyList, A]

    Properties.either(
      "Coproduct[NonEmptyList, NonEmptyList, ?]",
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testCoproductIListMaybe = {
    type F[A] = Coproduct[IList, Maybe, A]

    Properties.either(
      "Coproduct[IList, Maybe, ?]",
      scalazlaws.cobind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

}
