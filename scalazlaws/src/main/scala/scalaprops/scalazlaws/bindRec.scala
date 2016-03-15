package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object bindRec {
  def tailrecBindConsistency[F[_], A](implicit F: BindRec[F], G1: Gen[A], G2: Gen[A => F[A]], E: Equal[F[A]]) =
    forAll(F.bindRecLaw.tailrecBindConsistency[A] _)

  def handleManyBinds[F[_], A](bindCount: Int)(implicit F: BindRec[F], G: Gen[F[A]]) =
    forAll { fa: F[A] =>
      F.tailrecM[Int, A](0) { i =>
        if (i < bindCount)
          F.map(fa)(_ => \/.left(i + 1))
        else
          F.map(fa)(\/.right)
      }
      true
    }

  def laws[F[_]: BindRec](implicit G1: Gen[F[Int]], G2: Gen[Int => F[Int]], E: Equal[F[Int]]) =
    lawsWithCount[F](100000)

  def lawsWithCount[F[_]: BindRec](bindCount: Int)(implicit G1: Gen[F[Int]], G2: Gen[Int => F[Int]], E: Equal[F[Int]]) =
    Properties.fromChecks(ScalazLaw.bindRec)(
      ScalazLaw.bindRecTailrecBindConsistency -> Check(
        bindRec.tailrecBindConsistency[F, Int]
      ),
      ScalazLaw.bindRecHandleManyBinds -> Check(
        bindRec.handleManyBinds[F, Int](bindCount), Param.minSuccessful(1)
      )
    )

  def all[F[_]: BindRec](implicit G1: Gen[F[Int]], G2: Gen[Int => F[Int]], G3: Gen[F[Int => Int]], e: Equal[F[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.bindRecAll, bindRec.laws[F], bind.all[F])
}
