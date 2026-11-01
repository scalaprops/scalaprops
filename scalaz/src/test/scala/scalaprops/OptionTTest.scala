package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.option.*

object OptionTTest extends Scalaprops {
  property("disjunction") = {
    type E = Byte
    type G[A] = E \/ A
    type F[A] = OptionT[G, A]

    scalazlaws.monadError.all[F, E]
  }

  property("id") = {
    type F[A] = OptionT[Id.Id, A]

    Properties.list(
      scalazlaws.equal.all[F[Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.laws[F],
      scalazlaws.monad.all[F]
    )
  }

  property("iListBindRec") =
    scalazlaws.bindRec.laws[({ type l[a] = OptionT[IList, a] })#l].andThenParam(Param.maxSize(1))

  property("iList") = {
    type F[A] = OptionT[IList, A]
    Properties.list(
      scalazlaws.equal.all[OptionT[IList, Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  property("maybe") = {
    type F[A] = OptionT[Maybe, A]
    Properties.list(
      scalazlaws.equal.all[OptionT[Maybe, Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  property("monadTrans") = scalazlaws.monadTrans.all[OptionT]
}
