package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import ScalapropsScalaz._

object WriterTTest extends Scalaprops {

  val bindRecIList = scalazlaws.bindRec.laws[({ type l[a] = WriterT[IList, Byte, a] })#l].andThenParam(Param.maxSize(1))

  val testMaybe1 = {
    type F[A] = WriterT[Maybe, Int, A]

    Properties.list(
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testMaybe2 = {
    type F[A, B] = WriterT[Maybe, A, B]

    scalazlaws.bitraverse.all[F]
  }

  val iList1 = {
    type F[A] = WriterT[IList, Int, A]

    Properties.list(
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val tree = {
    type F[A] = WriterT[Tree, Byte, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val either = {
    type E = Byte
    type F[A] = E \/ A
    type G[A] = WriterT[F, Short, A]

    Properties.list(
      scalazlaws.monadError.all[G, E],
      scalazlaws.plus.all[G],
      scalazlaws.traverse.all[G],
      scalazlaws.equal.all[G[Int]]
    )
  }

  val id = {
    type F[A] = Writer[Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({ type l[f[_], a] = WriterT[f, Int, a] })#l]

}
