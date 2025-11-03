package scalaprops
package scalazlaws

import scalaz.*

object applicativePlus {
  def all[F[_]](implicit
    F: ApplicativePlus[F],
    af: Gen[F[Int]],
    aff: Gen[F[Int => Int]],
    e: Equal[F[Int]]
  ): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applicativePlus, applicative.all[F], plusEmpty.all[F])
}
