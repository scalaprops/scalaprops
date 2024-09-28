package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.either._
import scalaz.std.tuple._
import FunctionEqual._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object IndexedReaderWriterStateTTest extends Scalaprops {
  private[this] implicit def equal[F[_]: Monad, R, W, S1, S2, A](implicit
    F: Equal[(R, S1) => F[(W, A, S2)]]
  ): Equal[IndexedReaderWriterStateT[R, W, S1, S2, F, A]] =
    F.contramap(_.run)

  val id = {
    type F[A] = RWS[Byte, Short, Int, A]
    Properties.list(
      scalazlaws.bindRec.all[F],
      scalazlaws.monad.all[F]
    )
  }

  val bindRecIList = {
    scalazlaws.bindRec.laws[({ type l[a] = RWST[Byte, Byte, Byte, IList, a] })#l]
  }.andThenParam(Param.maxSize(1))

  val testMaybe = {
    type F[A] = IndexedReaderWriterStateT[Int, Int, Int, Int, Maybe, A]
    Properties.list(
      scalazlaws.bindRec.all[F],
      scalazlaws.monadPlusStrong.all[F]
    )
  }

  val testIList = {
    type F[A] = IndexedReaderWriterStateT[Int, Int, Int, Int, IList, A]
    scalazlaws.monadPlusStrong.all[F]
  }

  val tree = {
    type F[A] = IndexedReaderWriterStateT[Int, Int, Int, Int, Tree, A]
    scalazlaws.monad.all[F]
  }.composeParam(Param.minSuccessful(10) andThen Param.maxSize(10))

  val nel = {
    type F[A] = IndexedReaderWriterStateT[Int, Int, Int, Int, NonEmptyList, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({ type l[f[_], a] = ReaderWriterStateT[Int, Int, Int, f, a] })#l]

  val monadError = {
    type F[A] = RWST[Int, Int, Int, ({ type l[a] = Either[Int, a] })#l, A]
    scalazlaws.monadError.all[F, Int]
  }
}
