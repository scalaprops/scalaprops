package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object MaybeTTest extends Scalaprops {
  property("disjunction") = {
    type E = Byte
    type G[A] = E \/ A
    type F[A] = MaybeT[G, A]
    scalazlaws.monadError.all[F, E]
  }

  property("id") = {
    type F[A] = MaybeT[Id.Id, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.laws[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  property("bindRecIList") =
    scalazlaws.bindRec.laws[({ type l[a] = MaybeT[IList, a] })#l].andThenParam(Param.maxSize(1))

  property("lawsIList") = {
    type F[A] = MaybeT[IList, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  property("lawsMaybe") = {
    type F[A] = MaybeT[Maybe, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  property("monadTrans") = scalazlaws.monadTrans.all[MaybeT]
}
