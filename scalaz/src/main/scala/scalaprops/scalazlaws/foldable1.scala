package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._
import scalaz.std.anyVal._

object foldable1 {
  def leftFM1Consistent[F[_], A](implicit F: Foldable1[F], fa: Gen[F[A]], ea: Equal[A]) =
    forAll(F.foldable1Law.leftFM1Consistent[A] _)

  def rightFM1Consistent[F[_], A](implicit F: Foldable1[F], fa: Gen[F[A]], ea: Equal[A]) =
    forAll(F.foldable1Law.rightFM1Consistent[A] _)

  def laws[F[_]: Foldable1](implicit fa: Gen[F[Int]]) =
    Properties.properties(ScalazLaw.foldable1)(
      ScalazLaw.foldable1LeftFM1Consistent -> leftFM1Consistent[F, Int],
      ScalazLaw.foldable1RightFM1Consistent -> rightFM1Consistent[F, Int]
    )

  def all[F[_]: Foldable1](implicit fa: Gen[F[Int]]) =
    Properties.fromProps(ScalazLaw.foldable1All, foldable1.laws[F], foldable.laws[F])
}
