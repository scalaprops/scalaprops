package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.function.*

object FreeApTest extends Scalaprops {
  private[this] implicit def freeApEqual[F[_]: Functor, A](implicit
    E: Eq1[F],
    A: Equal[A]
  ): Equal[FreeAp[F, A]] =
    FreeTest.freeEqual[F, A].contramap(_.monadic)

  private[this] def freeApGen0[F[_], A](implicit
    G1: Gen[A],
    G2: Gen[F[A]]
  ): Gen[FreeAp[F, A]] =
    Gen.oneOf(
      G1.map(FreeAp.pure),
      G2.map(FreeAp.lift(_))
    )

  private[this] implicit def freeApGen[F[_], A: Gen](implicit
    G1: Gen[F[A]],
    G2: Gen[F[Byte]],
    G3: Gen[F[Byte => A]]
  ): Gen[FreeAp[F, A]] =
    Gen.oneOf(
      freeApGen0[F, A],
      Apply[Gen].apply2(G2, freeApGen0[F, Byte => A])(FreeAp.apply[F, Byte, A](_, _))
    )

  property("id") = {
    type F[A] = FreeAp[Id.Id, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.all[F]
    )
  }

  property("maybe") = {
    type F[A] = FreeAp[Maybe, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.all[F]
    )
  }

  property("iList") = {
    type F[A] = FreeAp[IList, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.all[F]
    )
  }

  property("nel") = {
    type F[A] = FreeAp[NonEmptyList, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.all[F]
    )
  }

  property("disjunction") = {
    type G[A] = Short \/ A
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.all[F]
    )
  }

  property("function1") = {
    type G[A] = Short => A
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.laws[F], // exclude applyComposition law. because too slow
      scalazlaws.functor.all[F]
    )
  }

  property("state") = {
    type G[A] = State[Short, A]
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.laws[F], // exclude applyComposition law. because too slow
      scalazlaws.functor.all[F]
    )
  }

  property("writer") = {
    type G[A] = Writer[Short, A]
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.all[F]
    )
  }

  property("writerMaybe") = {
    type G[A] = WriterT[Short, Maybe, A]
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.all[F]
    )
  }

  property("writerIList") = {
    type G[A] = WriterT[Short, IList, A]
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.applicative.all[F]
    )
  }
}
