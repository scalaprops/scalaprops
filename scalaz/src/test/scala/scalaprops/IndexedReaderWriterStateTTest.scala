package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import FunctionEqual._
import ScalapropsScalaz._

object IndexedReaderWriterStateTTest extends Scalaprops {

  private[this] implicit def equal[F[_]: Monad, R, W, S1, S2, A](
    implicit F: Equal[(R, S1) => F[(W, A, S2)]]
  ): Equal[IndexedReaderWriterStateT[F, R, W, S1, S2, A]] =
    F.contramap(_.run)

  val id = {
    type F[A] = RWS[Byte, Short, Int, A]
    Properties.list(
      scalazlaws.bindRec.all[F],
      scalazlaws.monad.all[F]
    )
  }

  val bindRecIList = {
    scalazlaws.bindRec.laws[({type l[a] = RWST[IList, Byte, Byte, Byte, a]})#l]
  }.andThenParam(Param.maxSize(1))

  val testMaybe = {
    type F[A] = IndexedReaderWriterStateT[Maybe, Int, Int, Int, Int, A]
    Properties.list(
      scalazlaws.bindRec.all[F],
      scalazlaws.monadPlusStrong.all[F]
    )
  }

  val testIList = {
    type F[A] = IndexedReaderWriterStateT[IList, Int, Int, Int, Int, A]
    scalazlaws.monadPlusStrong.all[F]
  }

  val tree = {
    type F[A] = IndexedReaderWriterStateT[Tree, Int, Int, Int, Int, A]
    scalazlaws.monad.all[F]
  }.composeParam(Param.minSuccessful(10) andThen Param.maxSize(10))

  val nel = {
    type F[A] = IndexedReaderWriterStateT[NonEmptyList, Int, Int, Int, Int, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = ReaderWriterStateT[f, Int, Int, Int, a]})#l]

}
