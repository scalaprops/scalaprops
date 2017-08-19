package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.function._

sealed trait FreeApInstances {

  implicit def freeApCobind[F[_]]: Cobind[({type l[a] = FreeAp[F, a]})#l] =
    new FreeApCobind[F] {}

}

object FreeApTest extends Scalaprops with FreeApInstances{

  private[this] implicit def freeApEqual[F[_]: Functor, A](implicit
    E: Eq1[F],
    A: Equal[A]
  ): Equal[FreeAp[F, A]] =
    FreeTest.freeEqual[F, A].contramap(_.monadic)

  private[this] def freeApGen0[F[_]: Functor, A](implicit
    G1: Gen[A],
    G2: Gen[F[A]]
  ): Gen[FreeAp[F, A]] =
    Gen.oneOf(
      G1.map(FreeAp.pure),
      G2.map(FreeAp.lift(_))
    )

  private[this] implicit def freeApGen[F[_]: Functor, A: Gen](implicit
    G1: Gen[F[A]],
    G2: Gen[F[Byte]],
    G3: Gen[F[Byte => A]]
  ): Gen[FreeAp[F, A]] = Gen.oneOf(
    freeApGen0[F, A],
    Apply[Gen].apply2(G2, freeApGen0[F, Byte => A])(FreeAp.apply[F, Byte, A](_, _))
  )

  implicit def freeApComonad[F[_]](implicit C: Comonad[F]): Comonad[({type l[a] = FreeAp[F, a]})#l] =
    new Comonad[({type l[a] = FreeAp[F, a]})#l] with FreeApCobind[F] {
      override def copoint[A](p: FreeAp[F, A]) =
        p.para[A](
          identity,
          new NaturalTransformation[
            ({type l[a] = (F[a], FreeAp[F, a => A])})#l,
            ({type l[a] = A})#l
          ] {
            def apply[B](fa: (F[B], FreeAp[F, B => A])) = {
              copoint(fa._2).apply(C.copoint(fa._1))
            }
          }
        )
    }

  implicit def cogenFreeAp[F[_]: Applicative, A](implicit C: Cogen[F[A]]): Cogen[FreeAp[F, A]] =
    C.contramap[FreeAp[F, A]](_.retract)

  val id = {
    type F[A] = FreeAp[Id.Id, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.comonad.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val maybe = {
    type F[A] = FreeAp[Maybe, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val iList = {
    type F[A] = FreeAp[IList, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val nel = {
    type F[A] = FreeAp[NonEmptyList, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.comonad.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val disjunction = {
    type G[A] = Short \/ A
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val function1 = {
    type G[A] = Short => A
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.laws[F], // exclude applyComposition law. because too slow
      scalazlaws.functor.all[F]
    )
  }

  val state = {
    type G[A] = State[Short, A]
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.laws[F], // exclude applyComposition law. because too slow
      scalazlaws.functor.all[F]
    )
  }

  val writer = {
    type G[A] = Writer[Short, A]
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val writerMaybe = {
    type G[A] = WriterT[Maybe, Short, A]
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.all[F]
    )
  }

  val writerIList = {
    type G[A] = WriterT[IList, Short, A]
    type F[A] = FreeAp[G, A]
    Properties.list(
      scalazlaws.equal.all[F[Byte]],
      scalazlaws.cobind.all[F],
      scalazlaws.applicative.all[F]
    )
  }

}

private trait FreeApCobind[F[_]] extends Cobind[({type l[a] = FreeAp[F, a]})#l] {
  override final def cobind[A, B] (fa: FreeAp[F, A])(f: FreeAp[F, A] => B) =
    FreeAp.point(f(fa))

  override final def map[A, B] (fa: FreeAp[F, A])(f: A => B) =
    fa map f
}

