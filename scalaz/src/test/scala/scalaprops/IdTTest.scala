package scalaprops

import scalaz._
import scalaz.Id.Id
import scalaz.std.anyVal._
import ScalapropsScalaz._

object IdTTest extends Scalaprops {

  val monadTrans = scalazlaws.monadTrans.all[IdT]

  val bindRecIList = scalazlaws.bindRec.laws[({ type l[a] = IdT[IList, a] })#l].andThenParam(Param.maxSize(1))

  val id = {
    type F[A] = IdT[Id, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.order.all[F[Byte]],
      scalazlaws.monad.all[F]
    )
  }

  val maybe = {
    type F[A] = IdT[Maybe, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.order.all[F[Byte]],
      scalazlaws.monad.all[F]
    )
  }

  val iList = {
    type F[A] = IdT[IList, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.order.all[F[Byte]],
      scalazlaws.monad.all[F]
    )
  }

}
