package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object OneAndTest extends Scalaprops {
  property("iList") = {
    type F[A] = OneAnd[IList, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.monad.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F],
      scalazlaws.plus.all[F]
    )
  }

  property("maybe") = {
    type F[A] = OneAnd[Maybe, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.monad.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F],
      scalazlaws.plus.all[F]
    )
  }

  property("nel") = {
    type F[A] = OneAnd[NonEmptyList, A]

    Properties.list(
      scalazlaws.order.all[F[Int]],
      scalazlaws.bind.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F],
      scalazlaws.plus.all[F]
    )
  }

  property("tree") = {
    type F[A] = OneAnd[Tree, A]

    Properties.list(
      scalazlaws.equal.all[F[Int]],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F]
    )
  }
}
