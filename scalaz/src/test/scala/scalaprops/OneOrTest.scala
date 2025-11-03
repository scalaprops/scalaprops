package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object OneOrTest extends Scalaprops {
  val testIList = {
    type F[A] = OneOr[IList, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.cobind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val testNel = {
    type F[A] = OneOr[NonEmptyList, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.applicative.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.comonad.all[F]
    )
  }
}
