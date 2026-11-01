package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object CoyonedaTest extends Scalaprops {
  property("orderMaybe") = scalazlaws.order.all[Coyoneda[Maybe, Int]]
  property("orderIList") = scalazlaws.order.all[Coyoneda[IList, Int]]

  property("nel") = {
    type F[A] = Coyoneda[NonEmptyList, A]

    Properties.list(
      scalazlaws.traverse1.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.laws[F].andThenParam(Param.maxSize(1)),
      scalazlaws.plus.all[F]
    )
  }

  property("maybe") = {
    type F[A] = Coyoneda[Maybe, A]

    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.cobind.all[F]
    )
  }

  property("iList") = {
    type F[A] = Coyoneda[IList, A]

    Properties.list(
      scalazlaws.bindRec.laws[F].andThenParam(Param.maxSize(1)),
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.cobind.all[F]
    )
  }
}
