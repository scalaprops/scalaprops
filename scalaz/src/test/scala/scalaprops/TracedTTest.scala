package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object TracedTTest extends Scalaprops {
  private[this] val e = new FunctionEqual(10)
  import e.*

  override def param = super.param.copy(maxSize = 10)

  type Z = IList[Boolean]

  property("trans") = scalazlaws.comonadTrans.all[({ type l[w[_], a] = TracedT[w, Z, a] })#l]

  property("contravariantMaybe") = scalazlaws.contravariant.all[({ type l[a] = TracedT[Maybe, a, Byte] })#l]
  property("contravariantIList") = scalazlaws.contravariant.all[({ type l[a] = TracedT[IList, a, Byte] })#l]
  property("contravariantTree") = scalazlaws.contravariant.all[({ type l[a] = TracedT[Tree, a, Byte] })#l]

  property("maybe") = {
    type F[A] = TracedT[Maybe, Z, A]
    Properties.list(
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  property("iList") = {
    type F[A] = TracedT[IList, Z, A]
    Properties.list(
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  property("nel") = {
    type F[A] = TracedT[NonEmptyList, Z, A]
    Properties.list(
      scalazlaws.comonad.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  property("oneOrNel") = {
    type G[A] = OneOr[NonEmptyList, A]
    type F[A] = TracedT[G, Z, A]
    Properties.list(
      scalazlaws.comonad.all[F](using
        TracedT.tracedTComonad[G, Z],
        Gen[TracedT[G, Z, Int]],
        Gen[TracedT[G, Z, Int] => Int],
        Equal[TracedT[G, Z, Int]]
      )
    )
  }

  property("tree") = {
    type F[A] = TracedT[Tree, Z, A]
    Properties.list(
      scalazlaws.comonad.all[F],
      scalazlaws.apply.all[F]
    )
  }

  property("store") = {
    type G[A] = Store[Int, A]
    type F[A] = TracedT[G, Z, A]
    import IndexedStoreTTest.indexedStoreTEqual

    scalazlaws.comonad.all[F](using
      TracedT.tracedTComonad[G, Z],
      Gen[TracedT[G, Z, Int]],
      Gen[TracedT[G, Z, Int] => Int],
      Equal[TracedT[G, Z, Int]]
    )
  }
}
