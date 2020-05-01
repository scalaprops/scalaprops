package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._
import scalaz.std.tuple._
import ScalapropsScalaz._

object traverse {
  def identityTraverse[F[_], X, Y](implicit F: Traverse[F], afx: Gen[F[X]], axy: Gen[X => Y], ef: Equal[F[Y]]) =
    forAll(F.traverseLaw.identityTraverse[X, Y] _)

  def purity[F[_], G[_], X](implicit f: Traverse[F], afx: Gen[F[X]], G: Applicative[G], ef: Equal[G[F[X]]]) =
    forAll(f.traverseLaw.purity[G, X] _)

  def sequentialFusion[F[_], N[_], M[_], A, B, C](implicit
    fa: Gen[F[A]],
    amb: Gen[A => M[B]],
    bnc: Gen[B => N[C]],
    F: Traverse[F],
    N: Applicative[N],
    M: Applicative[M],
    MN: Equal[M[N[F[C]]]]
  ): Property =
    forAll(F.traverseLaw.sequentialFusion[N, M, A, B, C] _)

  def naturality[F[_], N[_], M[_], A](
    nat: (M ~> N)
  )(implicit fma: Gen[F[M[A]]], F: Traverse[F], N: Applicative[N], M: Applicative[M], NFA: Equal[N[F[A]]]): Property =
    forAll(F.traverseLaw.naturality[N, M, A](nat) _)

  def parallelFusion[F[_], N[_], M[_], A, B](implicit
    fa: Gen[F[A]],
    amb: Gen[A => M[B]],
    anb: Gen[A => N[B]],
    F: Traverse[F],
    N: Applicative[N],
    M: Applicative[M],
    MN: Equal[(M[F[B]], N[F[B]])]
  ): Property =
    forAll(F.traverseLaw.parallelFusion[N, M, A, B] _)

  private[scalaprops] val maybe2ilist = new (Maybe ~> IList) {
    override def apply[A](fa: Maybe[A]) = fa.cata(IList.single, INil())
  }

  def laws[F[_]](implicit fa: Gen[F[Int]], F: Traverse[F], EF: Equal[F[Int]], GFM: Gen[F[Maybe[Int]]]) =
    Properties.fromChecks(ScalazLaw.traverse)(
      ScalazLaw.traverseIdentity -> Check(
        identityTraverse[F, Int, Int]
      ),
      ScalazLaw.traversePurityMaybe -> Check(
        purity[F, Maybe, Int]
      ),
      ScalazLaw.traversePurityIList -> Check(
        purity[F, IList, Int]
      ),
      ScalazLaw.traverseSequentialFusion -> Check(
        sequentialFusion[F, Maybe, IList, Int, Int, Int],
        Param.maxSize(3) andThen Param.minSuccessful(10)
      ),
      ScalazLaw.traverseNaturality -> Check(
        naturality[F, IList, Maybe, Int](maybe2ilist)
      ),
      ScalazLaw.traverseParallelFusion -> Check(
        parallelFusion[F, IList, Maybe, Int, Int],
        Param.maxSize(3)
      )
    )

  def all[F[_]](implicit F: Traverse[F], fa: Gen[F[Int]], EF: Equal[F[Int]], GFM: Gen[F[Maybe[Int]]]) =
    Properties.fromProps(ScalazLaw.traverseAll, traverse.laws[F], functor.all[F], foldable.all[F])
}
