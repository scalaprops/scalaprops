package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

object applicative {
  def identity[F[_], X](implicit f: Applicative[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(f.applicativeLaw.identityAp[X] _)

  def homomorphism[F[_], X, Y](implicit ap: Applicative[F], ax: Gen[X], af: Gen[X => Y], e: Equal[F[Y]]) =
    forAll(ap.applicativeLaw.homomorphism[X, Y] _)

  def interchange[F[_], X, Y](implicit ap: Applicative[F], ax: Gen[X], afx: Gen[F[X => Y]], e: Equal[F[Y]]) =
    forAll(ap.applicativeLaw.interchange[X, Y] _)

  def mapApConsistency[F[_], X, Y](implicit ap: Applicative[F], ax: Gen[F[X]], afx: Gen[X => Y], e: Equal[F[Y]]) =
    forAll(ap.applicativeLaw.mapLikeDerived[X, Y] _)

  def laws[F[_]](implicit F: Applicative[F], af: Gen[F[Int]],
                 aff: Gen[F[Int => Int]], e: Equal[F[Int]]) = properties(ScalazLaw.applicative)(
    ScalazLaw.applicativeIdentity -> applicative.identity[F, Int],
    ScalazLaw.applicativeHomomorphism -> applicative.homomorphism[F, Int, Int],
    ScalazLaw.applicativeInterchange -> applicative.interchange[F, Int, Int],
    ScalazLaw.applicativeMapConsistentWithAp -> applicative.mapApConsistency[F, Int, Int]
  )

  def all[F[_]](implicit F: Applicative[F], af: Gen[F[Int]],
                aff: Gen[F[Int => Int]], e: Equal[F[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applicativeAll, applicative.laws[F], apply.all[F])
}
