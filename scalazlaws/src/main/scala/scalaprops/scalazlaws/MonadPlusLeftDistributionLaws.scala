package scalaprops
package scalazlaws

import scalaz.{Equal, MonadPlus}

private[scalazlaws] object MonadPlusLeftDistributionLaws {
  def apply[F[_]](implicit F: MonadPlus[F]): MonadPlusLeftDistributionLaws[F] = new MonadPlusLeftDistributionLaws(F)
}

/**
  * @see [[https://wiki.haskell.org/MonadPlus_reform_proposal]]
  */
private[scalazlaws] class MonadPlusLeftDistributionLaws[F[_]] private (F: MonadPlus[F]) {

  def leftDistribution[A, B](fa: F[A], fb: F[A], k: A => F[B])(implicit E: Equal[F[B]]): Boolean =
    E.equal(F.bind(F.plus(fa, fb))(k), F.plus(F.bind(fa)(k), F.bind(fb)(k)))

}
