package scalaprops
package neko

import cats.FlatMap
import cats.laws.FlatMapLaws

object flatmap {

  def associativity[F[_]: FlatMap, A, B, C](implicit
    G1: Gen[F[A]], G2: Gen[A => F[B]], Gen3: Gen[B => F[C]], E: Eq[F[C]]
  ) = forAllNeko(FlatMapLaws[F].flatMapAssociativity[A, B, C] _)

  def consistentApply[F[_]: FlatMap, A, B](implicit G: Gen[F[A]], G2: Gen[F[A => B]], E: Eq[F[B]]) =
    forAllNeko(FlatMapLaws[F].flatMapConsistentApply[A, B] _)

  def kleisliAssociativity[F[_]: FlatMap, A: Gen, B, C, D](implicit
    E: Eq[F[D]], G1: Gen[A => F[B]], G2: Gen[B => F[C]], G3: Gen[C => F[D]]
  ) = forAllNeko(FlatMapLaws[F].kleisliAssociativity[A, B, C, D] _)

  def laws[F[_]: FlatMap](implicit G1: Gen[F[Byte]], G2: Gen[F[Byte => Byte]], E1: Eq[F[Byte]]) =
    Properties.properties(Neko.flatmap)(
      Neko.flatmapAssociativity -> associativity[F, Byte, Byte, Byte],
      Neko.flatmapConsistentApply -> consistentApply[F, Byte, Byte],
      Neko.flatmapKleisliAssociativity -> kleisliAssociativity[F, Byte, Byte, Byte, Byte]
    ).andThenParam(Param.maxSize(5))

  def all[F[_]: FlatMap](implicit G1: Gen[F[Byte]], G2: Gen[F[Byte => Byte]], E1: Eq[F[Byte]]) =
    Properties.fromProps(Neko.flatmapAll, laws[F], apply.all[F])

}
