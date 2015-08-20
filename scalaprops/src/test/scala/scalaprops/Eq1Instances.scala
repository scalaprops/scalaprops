package scalaprops

import scalaz._
import scalaz.std.tuple._

object Eq1Instances {

  implicit def f1Eq1[A: Gen]: Eq1[({type l[a] = A => a})#l] =
    new Eq1[({type l[a] = A => a})#l] {
      import FunctionEqual._
      def eq1[B: Equal] = Equal[A => B]
    }

  implicit def kleisliEq1[F[_], A: Gen](implicit F: Eq1[F]): Eq1[({type l[a] = Kleisli[F, A, a]})#l] =
    new Eq1[({type l[a] = Kleisli[F, A, a]})#l] {
      def eq1[B: Equal] = {
        import KleisliTest.kleisliEqual
        implicit val f: Equal[F[B]] = F.eq1[B]
        Equal[Kleisli[F, A, B]]
      }
    }

  implicit def stateEq1[F[_]: Monad: Eq1, S: Cogen: Gen: Equal]: Eq1[({type l[a] = StateT[F, S, a]})#l] =
    new Eq1[({type l[a] = StateT[F, S, a]})#l] {
      import FunctionEqual._
      def eq1[A: Equal] = {
        implicit val e = Eq1[F].eq1[(S, A)]
        StateTTest.stateTEqual[F, S, A]
      }
    }
}
