package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaz.*

object align {
  def collapse[F[_], A](implicit F: Align[F], E: Equal[F[A \&/ A]], A: Gen[F[A]]) =
    forAll(F.alignLaw.collapse[A] _)

  def laws[F[_]](implicit F: Align[F], af: Gen[F[Int]], ef: Equal[F[Int \&/ Int]]) =
    properties(ScalazLaw.align)(
      ScalazLaw.alignCollapse -> collapse[F, Int]
    )

  def all[F[_]: Align](implicit af: Gen[F[Int]], e: Equal[F[Int]], ef: Equal[F[Int \&/ Int]]) =
    Properties.fromProps(ScalazLaw.alignAll, functor.all[F], align.laws[F])
}
