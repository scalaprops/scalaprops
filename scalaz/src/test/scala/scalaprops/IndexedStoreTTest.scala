package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object IndexedStoreTTest extends Scalaprops {
  private[this] val e = new FunctionEqual(10)
  import e._

  implicit def indexedStoreTEqual[F[_], I: Equal, A, B](implicit
    F: Equal[F[A => B]]
  ): Equal[IndexedStoreT[F, I, A, B]] =
    Equal[(F[A => B], I)].contramap(_.run)

  val testMaybe1 = scalazlaws.contravariant.all[({ type l[A] = IndexedStoreT[Maybe, Int, A, Int] })#l]
  val testIList1 = scalazlaws.contravariant.all[({ type l[A] = IndexedStoreT[IList, Int, A, Int] })#l]

  val testMaybe2 = scalazlaws.bifunctor.all[({ type l[A, B] = IndexedStoreT[Maybe, A, Int, B] })#l]
  val testIList2 = scalazlaws.bifunctor.all[({ type l[A, B] = IndexedStoreT[IList, A, Int, B] })#l]

  val testNel = scalazlaws.comonad.all[({ type l[A] = StoreT[NonEmptyList, Int, A] })#l]
  val testMaybe3 = scalazlaws.cobind.all[({ type l[A] = StoreT[Maybe, Int, A] })#l]
  val testIList3 = scalazlaws.cobind.all[({ type l[A] = StoreT[IList, Int, A] })#l]

  val comonadTransLaws = scalazlaws.comonadTrans.all[({ type l[f[_], a] = StoreT[f, Int, a] })#l]
}
