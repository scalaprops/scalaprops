package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

object WriterTTest extends Scalaprops {
  property("bindRecIList") =
    scalazlaws.bindRec.laws[({ type l[a] = WriterT[Byte, IList, a] })#l].andThenParam(Param.maxSize(1))

  property("maybe1") = {
    type F[A] = WriterT[Int, Maybe, A]

    Properties.list(
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("maybe2") = {
    type F[A, B] = WriterT[A, Maybe, B]

    scalazlaws.bitraverse.all[F]
  }

  property("iList1") = {
    type F[A] = WriterT[Int, IList, A]

    Properties.list(
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("tree") = {
    type F[A] = WriterT[Byte, Tree, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("either") = {
    type E = Byte
    type F[A] = E \/ A
    type G[A] = WriterT[Short, F, A]

    Properties.list(
      scalazlaws.monadError.all[G, E],
      scalazlaws.plus.all[G],
      scalazlaws.traverse.all[G],
      scalazlaws.equal.all[G[Int]]
    )
  }

  property("id") = {
    type F[A] = Writer[Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("monadTrans") = scalazlaws.monadTrans.all[({ type l[f[_], a] = WriterT[Int, f, a] })#l]
}
