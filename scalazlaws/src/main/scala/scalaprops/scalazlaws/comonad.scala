package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._
import scalaz.std.anyVal._

object comonad {

  def cobindLeftIdentity[F[_], A](implicit F: Comonad[F], F0: Equal[F[A]], fa: Gen[F[A]]): Property =
    forAll(F.comonadLaw.cobindLeftIdentity[A] _)

  def cobindRightIdentity[F[_], A, B](implicit F: Comonad[F], F0: Equal[B], fa: Gen[F[A]], f: Gen[F[A] => B]): Property =
    forAll(F.comonadLaw.cobindRightIdentity[A, B] _)

  def laws[F[_]](implicit a: Comonad[F], am: Gen[F[Int]],
                 af: Gen[F[Int] => Int], e: Equal[F[Int]]): Properties[ScalazLaw] =
    properties(ScalazLaw.comonad)(
      ScalazLaw.comonadLeftIdentity -> cobindLeftIdentity[F, Int],
      ScalazLaw.comonadRightIdentity -> cobindRightIdentity[F, Int, Int]
    )

  def all[F[_]](implicit a: Comonad[F], am: Gen[F[Int]],
                af: Gen[F[Int] => Int], e: Equal[F[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.comonadAll, comonad.laws[F], cobind.all[F])
}
