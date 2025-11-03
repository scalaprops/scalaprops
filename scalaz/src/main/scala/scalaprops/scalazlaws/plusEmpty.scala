package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaz.*

object plusEmpty {
  def leftPlusIdentity[F[_], X](implicit f: PlusEmpty[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(f.plusEmptyLaw.leftPlusIdentity[X] _)

  def rightPlusIdentity[F[_], X](implicit f: PlusEmpty[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(f.plusEmptyLaw.rightPlusIdentity[X] _)

  def laws[F[_]](implicit F: PlusEmpty[F], afx: Gen[F[Int]], ef: Equal[F[Int]]) =
    properties(ScalazLaw.plusEmpty)(
      ScalazLaw.plusEmptyLeftIdentity -> leftPlusIdentity[F, Int],
      ScalazLaw.plusEmptyRightIdentity -> rightPlusIdentity[F, Int]
    )

  def all[F[_]](implicit F: PlusEmpty[F], afx: Gen[F[Int]], ef: Equal[F[Int]]) = {
    implicit val m = F.monoid[Int]
    Properties.fromProps(ScalazLaw.plusEmptyAll, plusEmpty.laws[F], plus.laws[F], monoid.laws[F[Int]])
  }
}
