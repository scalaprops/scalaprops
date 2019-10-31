package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._
import scalaz.std.anyVal._

object cobind {
  def cobindAssociative[F[_], A, B, C, D](
    implicit F: Cobind[F],
    D: Equal[D],
    fa: Gen[F[A]],
    f: Gen[F[A] => B],
    g: Gen[F[B] => C],
    h: Gen[F[C] => D]
  ): Property =
    forAll(F.cobindLaw.cobindAssociative[A, B, C, D] _)

  def laws[F[_]](implicit a: Cobind[F], am: Gen[F[Int]], g: Gen[F[Int] => Int]): Properties[ScalazLaw] =
    Properties.fromChecks(ScalazLaw.cobind)(
      ScalazLaw.cobindAssociative -> Check(
        cobindAssociative[F, Int, Int, Int, Int],
        Param.maxSize(10)
      )
    )

  def all[F[_]](
    implicit a: Cobind[F],
    am: Gen[F[Int]],
    e: Equal[F[Int]],
    g: Gen[F[Int] => Int]
  ): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.cobindAll, cobind.laws[F], functor.all[F])
}
