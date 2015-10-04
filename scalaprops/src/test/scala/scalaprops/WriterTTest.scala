package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object WriterTTest extends Scalaprops {

  val testMaybe1 = {
    type F[A] = WriterT[Maybe, Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
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
      scalazlaws.monad.all[F],
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
    type F[A] = Byte \/ A
    type G[A] = WriterT[F, Short, A]

    implicit def writerTGen0[H[_, _], E, W, A](implicit F: Gen[H[E, (W, A)]]) =
      Gen.writerTGen[({type l[a] = H[E, a]})#l, W, A]

    implicit def writerTEqual0[H[_, _], E, W, A](implicit F: Equal[H[E, (W, A)]]) =
      WriterT.writerTEqual[({type l[a] = H[E, a]})#l, W, A]

    Properties.list(
      scalazlaws.monadError.all[({type x[e, a] = WriterT[({type y[b] = e \/ b})#y, Short, a]})#x, Byte],
      scalazlaws.traverse.all[G],
      scalazlaws.equal.all[G[Int]]
    )
  }

  val id = {
    type F[A] = Writer[Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = WriterT[f, Int, a]})#l]

}
