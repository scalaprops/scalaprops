package scalaprops

import scalaz._
import scalaz.std.AllInstances._

trait Eq1[F[_]] {
  def eq1[A: Equal]: Equal[F[A]]
}

object Eq1 {
  def apply[F[_]](implicit F: Eq1[F]) = F

  implicit def freeEq1[F[_]: Functor](implicit F: Eq1[F]): Eq1[({ type l[a] = Free[F, a] })#l] =
    new Eq1[({ type l[a] = Free[F, a] })#l] {
      def eq1[A: Equal] =
        new Equal[Free[F, A]] {
          def equal(a: Free[F, A], b: Free[F, A]) = {
            implicit val s: Equal[F[Free[F, A]]] = F.eq1[Free[F, A]](freeEq1[F].eq1)
            Equal[F[Free[F, A]] \/ A].equal(a.resume, b.resume)
          }
        }
    }

  implicit val idEq1: Eq1[Id.Id] =
    new Eq1[Id.Id] {
      def eq1[A: Equal] = Equal[A]
    }

  implicit val maybeEq1: Eq1[Maybe] =
    new Eq1[Maybe] {
      def eq1[A: Equal] = Equal[Maybe[A]]
    }

  implicit val treeEq1: Eq1[Tree] =
    new Eq1[Tree] {
      def eq1[A: Equal] = Equal[Tree[A]]
    }

  implicit val streamEq1: Eq1[Stream] =
    new Eq1[Stream] {
      def eq1[A: Equal] = Equal[Stream[A]]
    }

  implicit val iListEq1: Eq1[IList] =
    new Eq1[IList] {
      def eq1[A: Equal] = Equal[IList[A]]
    }

  implicit val nelEq1: Eq1[NonEmptyList] =
    new Eq1[NonEmptyList] {
      def eq1[A: Equal] = Equal[NonEmptyList[A]]
    }

  implicit def writerTEq1[F[_], W](implicit F: Eq1[F], W: Equal[W]): Eq1[({ type l[a] = WriterT[W, F, a] })#l] =
    new Eq1[({ type l[a] = WriterT[W, F, a] })#l] {
      def eq1[A: Equal] = {
        implicit val e: Equal[F[(W, A)]] = F.eq1[(W, A)]
        Equal[WriterT[W, F, A]]
      }
    }

  implicit def disjunctionEq1[A: Equal]: Eq1[({ type l[a] = A \/ a })#l] =
    new Eq1[({ type l[a] = A \/ a })#l] {
      def eq1[B: Equal] = Equal[A \/ B]
    }

  implicit def validationEq1[A: Equal]: Eq1[({ type l[a] = Validation[A, a] })#l] =
    new Eq1[({ type l[a] = Validation[A, a] })#l] {
      def eq1[B: Equal] = Equal[Validation[A, B]]
    }

  implicit def f1Eq1[A: Gen]: Eq1[({ type l[a] = A => a })#l] =
    new Eq1[({ type l[a] = A => a })#l] {
      import FunctionEqual._
      def eq1[B: Equal] = Equal[A => B]
    }

  implicit def kleisliEq1[F[_], A: Gen](implicit F: Eq1[F]): Eq1[({ type l[a] = Kleisli[F, A, a] })#l] =
    new Eq1[({ type l[a] = Kleisli[F, A, a] })#l] {
      def eq1[B: Equal] = {
        import KleisliTest.kleisliEqual
        implicit val f: Equal[F[B]] = F.eq1[B]
        Equal[Kleisli[F, A, B]]
      }
    }

  implicit def stateEq1[F[_]: Monad: Eq1, S: Cogen: Gen: Equal]: Eq1[({ type l[a] = StateT[S, F, a] })#l] =
    new Eq1[({ type l[a] = StateT[S, F, a] })#l] {
      import FunctionEqual._
      def eq1[A: Equal] = {
        implicit val e: Equal[F[(S, A)]] = Eq1[F].eq1[(S, A)]
        StateTTest.stateTEqual[F, S, A]
      }
    }
}
