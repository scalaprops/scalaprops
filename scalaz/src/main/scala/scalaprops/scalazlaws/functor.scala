package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

object TestNewLaw {

  import scalaz.std.anyVal._

  trait Ev1[T[_], F[_]] {
    def ev[A: T]: T[F[A]]
  }

  final case class Testable1[F[_]](gen: Ev1[Gen, F], cogen: Ev1[Cogen, F], equal: Ev1[Equal, F])

  sealed abstract class Type1With[Ev[_[_]]] {
    type Type1[_]
    def evidence: Ev[Type1]
  }

  type Kind = Type1With[Testable1]

  sealed abstract class LawCase{
    type A
    def equal: Equal[A]
    def lhs: A
    def rhs: A
  }

  object LawCase {
    def apply[A0: Equal](lhs0: A0, rhs0: A0): LawCase {type A = A0} =
      new LawCase {
        type A = A0
        def equal = Equal[A]
        def lhs = lhs0
        def rhs = rhs0
      }
  }

  type X = Int

  def identity(k: Kind)(fk: Functor[k.Type1]): Gen[LawCase] = {

    k.evidence.gen.ev(Gen[X]).map { ka =>
      val lhs = fk.map(ka)(x => x)
      val rhs = ka
      LawCase(lhs, rhs)(k.evidence.equal.ev[X])
    }
  }

}

object functor {

  def identity[F[_], X](implicit F: Functor[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(F.functorLaw.identity[X] _)

  def composite[F[_], X, Y, Z](implicit F: Functor[F], af: Gen[F[X]], axy: Gen[X => Y],
                               ayz: Gen[Y => Z], ef: Equal[F[Z]]) =
    forAll(F.functorLaw.composite[X, Y, Z] _)

  def laws[F[_]](implicit F: Functor[F], af: Gen[F[Int]], axy: Gen[Int => Int],
                 ef: Equal[F[Int]]) =
    properties(ScalazLaw.functor)(
      ScalazLaw.functorIdentity -> identity[F, Int],
      ScalazLaw.functorComposite -> composite[F, Int, Int, Int]
    )

  def all[F[_]](implicit F: Functor[F], af: Gen[F[Int]], axy: Gen[Int => Int],
                ef: Equal[F[Int]]) =
    Properties.fromProps(ScalazLaw.functorAll, functor.laws[F], invariantFunctor.laws[F])
}
