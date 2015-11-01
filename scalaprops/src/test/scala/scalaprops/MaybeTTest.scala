package scalaprops

import scalaz._
import scalaz.std.anyVal._

object MaybeTTest extends Scalaprops {

  val disjunction = {
    type E = Byte
    type G[A] = E \/ A
    type F[A] = MaybeT[G, A]
    scalazlaws.monadError.all[F, E]
  }

  val id = {
    type F[A] = MaybeT[Id.Id, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.laws[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val bindRecIList =
    scalazlaws.bindRec.laws[({type l[a] = MaybeT[IList, a]})#l].andThenParam(Param.maxSize(1))

  val testLawsIList = {
    type F[A] = MaybeT[IList, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val testLawsMaybe = {
    type F[A] = MaybeT[Maybe, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[MaybeT]

}
