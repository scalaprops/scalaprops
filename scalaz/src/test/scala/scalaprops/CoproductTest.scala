package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object CoproductTest extends Scalaprops {
  val testCoproductNelNel = {
    type F[A] = Coproduct[NonEmptyList, NonEmptyList, A]

    Properties.list(
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testCoproductIListMaybe = {
    type F[A] = Coproduct[IList, Maybe, A]

    Properties.list(
      scalazlaws.cobind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }
}
