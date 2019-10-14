package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object isEmpty {

  def emptyIsEmpty[F[_], X](implicit F: IsEmpty[F]) =
    forAll(F.isEmptyLaw.emptyIsEmpty[X])

  def emptyPlusIdentity[F[_], X](implicit f: IsEmpty[F], afx: Gen[F[X]]) =
    forAll(f.isEmptyLaw.emptyPlusIdentity[X] _)

  def laws[F[_]](implicit F: IsEmpty[F], afx: Gen[F[Int]]) =
    Properties.properties(ScalazLaw.isEmpty)(
      ScalazLaw.isEmptyEmptyIsEmpty -> emptyIsEmpty[F, Int],
      ScalazLaw.isEmptyEmptyPlusIdentity -> emptyPlusIdentity[F, Int]
    )

  def all[F[_]](implicit F: IsEmpty[F], afx: Gen[F[Int]], ef: Equal[F[Int]]) =
    Properties.fromProps(ScalazLaw.isEmptyAll, isEmpty.laws[F], plusEmpty.all[F])
}
