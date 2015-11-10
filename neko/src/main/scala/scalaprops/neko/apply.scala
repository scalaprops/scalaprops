package scalaprops
package neko

import cats.Apply
import cats.laws.ApplyLaws

object apply {

  def composition[F[_]: Apply, A, B, C](implicit
    G1: Gen[F[A]], G2: Gen[F[A => B]], G3: Gen[F[B => C]], E1: Eq[F[C]]
  ) = forAllNeko(ApplyLaws[F].applyComposition[A, B, C] _)

  def laws[F[_]: Apply](implicit G1: Gen[F[Byte]], G2: Gen[F[Byte => Byte]], E1: Eq[F[Byte]]) =
    Properties.fromChecks(Neko.apply)(
      Neko.applyComposition -> Check(
        composition[F, Byte, Byte, Byte], Param.maxSize(5)
      )
    )

  def all[F[_]: Apply](implicit G1: Gen[F[Byte]], G2: Gen[F[Byte => Byte]], E1: Eq[F[Byte]]) =
    Properties.fromProps(Neko.applyAll, apply.laws[F], functor.all[F])

}
