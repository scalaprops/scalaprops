package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.Id.Id
import scalaz.std.anyVal.*

object IdTTest extends Scalaprops {
  property("monadTrans") = scalazlaws.monadTrans.all[IdT]

  property("bindRecIList") = scalazlaws.bindRec.laws[({ type l[a] = IdT[IList, a] })#l].andThenParam(Param.maxSize(1))

  property("id") = {
    type F[A] = IdT[Id, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.order.all[F[Byte]],
      scalazlaws.monad.all[F]
    )
  }

  property("maybe") = {
    type F[A] = IdT[Maybe, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.order.all[F[Byte]],
      scalazlaws.monad.all[F]
    )
  }

  property("iList") = {
    type F[A] = IdT[IList, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.order.all[F[Byte]],
      scalazlaws.monad.all[F]
    )
  }
}
