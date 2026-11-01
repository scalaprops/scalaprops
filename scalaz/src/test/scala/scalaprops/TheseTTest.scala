package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object TheseTTest extends Scalaprops {
  private[this] type E = IList[Byte]

  property("idSemigroup") = {
    type F[A] = A
    Properties.list(
      scalazlaws.semigroup.all[TheseT[F, E, E]]
    )
  }
  property("maybeSemigroup") = {
    type F[A] = Maybe[A]
    Properties.list(
      scalazlaws.semigroup.all[TheseT[F, E, E]]
    )
  }
  property("iListSemigroup") = {
    type F[A] = IList[A]
    Properties
      .list(
        scalazlaws.semigroup.all[TheseT[F, E, E]]
      )
      .andThenParam(Param.maxSize(10))
  }
  property("disjunctionSemigroup") = {
    type F[A] = E \/ A
    Properties.list(
      scalazlaws.semigroup.all[TheseT[F, E, E]]
    )
  }

  property("id2") = {
    type F[A] = A
    type G[A, B] = TheseT[F, A, B]
    scalazlaws.bitraverse.all[G]
  }
  property("maybe2") = {
    type F[A] = Maybe[A]
    type G[A, B] = TheseT[F, A, B]
    scalazlaws.bitraverse.all[G]
  }
  property("iList2") = {
    type F[A] = IList[A]
    type G[A, B] = TheseT[F, A, B]
    scalazlaws.bitraverse.all[G]
  }
  property("disjunction2") = {
    type F[A] = E \/ A
    type G[A, B] = TheseT[F, A, B]
    scalazlaws.bitraverse.all[G]
  }

  property("id1") = {
    type F[A] = A
    type G[A] = TheseT[F, E, A]
    Properties.list(
      scalazlaws.traverse.all[G],
      scalazlaws.monad.all[G]
    )
  }
  property("maybe1") = {
    type F[A] = Maybe[A]
    type G[A] = TheseT[F, E, A]
    Properties.list(
      scalazlaws.traverse.all[G],
      scalazlaws.monad.all[G]
    )
  }
  property("iList1") = {
    type F[A] = IList[A]
    type G[A] = TheseT[F, E, A]
    Properties.list(
      scalazlaws.traverse.all[G],
      scalazlaws.monad.all[G]
    )
  }
  property("disjunction1") = {
    type F[A] = E \/ A
    type G[A] = TheseT[F, E, A]
    Properties.list(
      scalazlaws.traverse.all[G],
      scalazlaws.monad.all[G]
    )
  }

  property("monadTrans") = scalazlaws.monadTrans.all[({ type l[f[_], a] = TheseT[f, E, a] })#l]
}
