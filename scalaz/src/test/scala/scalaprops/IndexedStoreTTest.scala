package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

object IndexedStoreTTest extends Scalaprops {
  private[this] val e = new FunctionEqual(10)
  import e.*

  implicit def indexedStoreTEqual[F[_], I: Equal, A, B](implicit
    F: Equal[F[A => B]]
  ): Equal[IndexedStoreT[F, I, A, B]] =
    Equal[(F[A => B], I)].contramap(_.run)

  property("maybe1") = scalazlaws.contravariant.all[({ type l[A] = IndexedStoreT[Maybe, Int, A, Int] })#l]
  property("iList1") = scalazlaws.contravariant.all[({ type l[A] = IndexedStoreT[IList, Int, A, Int] })#l]

  property("maybe2") = scalazlaws.bifunctor.all[({ type l[A, B] = IndexedStoreT[Maybe, A, Int, B] })#l]
  property("iList2") = scalazlaws.bifunctor.all[({ type l[A, B] = IndexedStoreT[IList, A, Int, B] })#l]

  property("nel") = scalazlaws.comonad.all[({ type l[A] = StoreT[NonEmptyList, Int, A] })#l]
  property("maybe3") = scalazlaws.cobind.all[({ type l[A] = StoreT[Maybe, Int, A] })#l]
  property("iList3") = scalazlaws.cobind.all[({ type l[A] = StoreT[IList, Int, A] })#l]

  property("comonadTransLaws") = scalazlaws.comonadTrans.all[({ type l[f[_], a] = StoreT[f, Int, a] })#l]
}
