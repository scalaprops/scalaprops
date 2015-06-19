package scalaprops

import scalaz._
import scalaz.Id.Id
import scalaz.std.anyVal._

object IdTTest extends Scalaprops {

  val monadTrans = scalazlaws.monadTrans.all[IdT]

  val id = {
    type F[A] = IdT[Id, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.monad.all[F]
    )
  }

  val maybe = {
    type F[A] = IdT[Maybe, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.monad.all[F]
    )
  }

  val iList = {
    type F[A] = IdT[IList, A]
    Properties.list(
      scalazlaws.traverse.all[F],
      scalazlaws.monad.all[F]
    )
  }

}
