package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz.{Equal, MonadPlus}

object monadPlusLeftDistribution {

  def leftDistribution[F[_], A, B](implicit F: MonadPlus[F], G1: Gen[F[A]], G2: Gen[A => F[B]], E: Equal[F[B]]): Property =
    forAll(MonadPlusLeftDistributionLaws[F].leftDistribution[A, B] _)

  def laws[F[_]](implicit F: MonadPlus[F], G1: Gen[F[Int]], E: Equal[F[Int]]): Properties[ScalazLaw] =
    leftDistribution[F, Int, Int].toProperties(ScalazLaw.monadPlusLeftDistribution)

  def all[F[_]](implicit F: MonadPlus[F], G1: Gen[F[Int]], G2: Gen[F[Int => Int]], E: Equal[F[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(
      ScalazLaw.monadPlusLeftDistributionAll,
      monadPlusLeftDistribution.laws[F],
      monadPlus.all[F]
    )
}
