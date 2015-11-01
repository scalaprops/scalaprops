package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.option._

object OptionTTest extends Scalaprops{

  val disjunction = {
    type E = Byte
    type G[A] = E \/ A
    type F[A] = OptionT[G, A]

    scalazlaws.monadError.all[F, E]
  }

  val id = {
    type F[A] = OptionT[Id.Id, A]

    Properties.list(
      scalazlaws.equal.all[F[Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.laws[F],
      scalazlaws.monad.all[F]
    )
  }

  val iListBindRec = scalazlaws.bindRec.laws[({type l[a] = OptionT[IList, a]})#l].andThenParam(Param.maxSize(1))

  val iList = {
    type F[A] = OptionT[IList, A]
    Properties.list(
      scalazlaws.equal.all[OptionT[IList, Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  val maybe = {
    type F[A] = OptionT[Maybe, A]
    Properties.list(
      scalazlaws.equal.all[OptionT[Maybe, Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[OptionT]

}
