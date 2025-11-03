package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object TheseTTest extends Scalaprops {
  private[this] type E = IList[Byte]

  val idSemigroup = {
    type F[A] = A
    Properties.list(
      scalazlaws.semigroup.all[TheseT[F, E, E]]
    )
  }
  val maybeSemigroup = {
    type F[A] = Maybe[A]
    Properties.list(
      scalazlaws.semigroup.all[TheseT[F, E, E]]
    )
  }
  val iListSemigroup = {
    type F[A] = IList[A]
    Properties
      .list(
        scalazlaws.semigroup.all[TheseT[F, E, E]]
      )
      .andThenParam(Param.maxSize(10))
  }
  val disjunctionSemigroup = {
    type F[A] = E \/ A
    Properties.list(
      scalazlaws.semigroup.all[TheseT[F, E, E]]
    )
  }

  val id2 = {
    type F[A] = A
    type G[A, B] = TheseT[F, A, B]
    scalazlaws.bitraverse.all[G]
  }
  val maybe2 = {
    type F[A] = Maybe[A]
    type G[A, B] = TheseT[F, A, B]
    scalazlaws.bitraverse.all[G]
  }
  val iList2 = {
    type F[A] = IList[A]
    type G[A, B] = TheseT[F, A, B]
    scalazlaws.bitraverse.all[G]
  }
  val disjunction2 = {
    type F[A] = E \/ A
    type G[A, B] = TheseT[F, A, B]
    scalazlaws.bitraverse.all[G]
  }

  val id1 = {
    type F[A] = A
    type G[A] = TheseT[F, E, A]
    Properties.list(
      scalazlaws.traverse.all[G],
      scalazlaws.monad.all[G]
    )
  }
  val maybe1 = {
    type F[A] = Maybe[A]
    type G[A] = TheseT[F, E, A]
    Properties.list(
      scalazlaws.traverse.all[G],
      scalazlaws.monad.all[G]
    )
  }
  val iList1 = {
    type F[A] = IList[A]
    type G[A] = TheseT[F, E, A]
    Properties.list(
      scalazlaws.traverse.all[G],
      scalazlaws.monad.all[G]
    )
  }
  val disjunction1 = {
    type F[A] = E \/ A
    type G[A] = TheseT[F, E, A]
    Properties.list(
      scalazlaws.traverse.all[G],
      scalazlaws.monad.all[G]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({ type l[f[_], a] = TheseT[f, E, a] })#l]
}
