package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import ScalapropsScalaz._

object traverse1 {
  def identityTraverse1[F[_], X, Y](implicit f: Traverse1[F], afx: Gen[F[X]], axy: Gen[X => Y], ef: Equal[F[Y]]) =
    forAll(f.traverse1Law.identityTraverse1[X, Y] _)

  def sequentialFusion1[F[_], N[_], M[_], A, B, C](implicit fa: Gen[F[A]], amb: Gen[A => M[B]], bnc: Gen[B => N[C]],
                                                   F: Traverse1[F], N: Apply[N], M: Apply[M], MN: Equal[M[N[F[C]]]]) =
    forAll(F.traverse1Law.sequentialFusion1[N, M, A, B, C] _)

  def naturality1[F[_], N[_], M[_], A](nat: (M ~> N))
                                      (implicit fma: Gen[F[M[A]]], F: Traverse1[F], N: Apply[N], M: Apply[M], NFA: Equal[N[F[A]]]) =
    forAll(F.traverse1Law.naturality1[N, M, A](nat) _)

  def parallelFusion1[F[_], N[_], M[_], A, B](implicit fa: Gen[F[A]], amb: Gen[A => M[B]], anb: Gen[A => N[B]],
                                              F: Traverse1[F], N: Apply[N], M: Apply[M], MN: Equal[(M[F[B]], N[F[B]])]) =
    forAll(F.traverse1Law.parallelFusion1[N, M, A, B] _)

  def laws[F[_]](implicit fa: Gen[F[Int]], F: Traverse1[F], EF: Equal[F[Int]], GI: Gen[F[Maybe[Int]]], GIE: Gen[Int => Int \/ Int]) =
    Properties.fromChecks(ScalazLaw.traverse1) (
      ScalazLaw.traverse1Identity -> Check(
        identityTraverse1[F, Int, Int]
      ),
      ScalazLaw.traverse1SequentialFusion1 -> Check(
        sequentialFusion1[F, Maybe, IList, Int, Int, Int], Param.maxSize(3) andThen Param.minSuccessful(10)
      ),
      ScalazLaw.traverse1Naturality1 -> Check(
        naturality1[F, IList, Maybe, Int](traverse.maybe2ilist)
      ),
      ScalazLaw.traverse1ParallelFusion1 -> Check(
        parallelFusion1[NonEmptyList, Maybe, ({type l[a] = Int \/ a})#l, Int, Int], Param.maxSize(3)
      )
    )

  def all[F[_]](implicit fa: Gen[F[Int]], F: Traverse1[F], EF: Equal[F[Int]], GI: Gen[F[Maybe[Int]]], GIE: Gen[Int => Int \/ Int]) =
    Properties.fromProps(ScalazLaw.traverse1All, traverse1.laws[F], traverse.all[F], foldable1.all[F])
}
