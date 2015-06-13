package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object IndexedReaderWriterStateTTest extends Scalaprops {
  private[this] val e = new FunctionEqual(10)
  import e._

  private[this] implicit def equal[F[_], R, W, S1, S2, A](
    implicit F: Equal[(R, S1) => F[(W, A, S2)]]
  ): Equal[IndexedReaderWriterStateT[F, R, W, S1, S2, A]] =
    F.contramap(_.run)

  val testMaybe = {
    type F[A] = IndexedReaderWriterStateT[Maybe, Int, Int, Int, Int, A]
    scalazlaws.monad.all[F]
  }

  val testIList = {
    type F[A] = IndexedReaderWriterStateT[IList, Int, Int, Int, Int, A]
    scalazlaws.monad.all[F]
  }

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = ReaderWriterStateT[f, Int, Int, Int, a]})#l]

}
