package scalaprops

import scalaz._
import scalaz.std.anyVal._

object MaybeTTest extends Scalaprops {

  val disjunction = {
    type F[E, A] = MaybeT[({type l[a] = E \/ a})#l, A]

    implicit def gen[G[_, _], E, A](implicit G: Gen[G[E, Maybe[A]]]) =
      Gen.maybeTGen[({type l[a] = G[E, a]})#l, A]

    implicit def equal[G[_, _], E, A](implicit G: Equal[G[E, Maybe[A]]]) =
      MaybeT.maybeTEqual[({type l[a] = G[E, a]})#l, A]

    scalazlaws.monadError.all[F, Byte]
  }

  val testLawsIList = {
    type F[A] = MaybeT[IList, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val testLawsMaybe = {
    type F[A] = MaybeT[Maybe, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[MaybeT]

  import FunctionEqual._

  val hoist1 = scalazlaws.hoist.law1[MaybeT, Maybe, Maybe, Maybe]
  val hoist2 = scalazlaws.hoist.law1[MaybeT, IList, IList, IList]
  val hoist3 = scalazlaws.hoist.law2[MaybeT, Maybe]
  val hoist4 = scalazlaws.hoist.law2[MaybeT, IList]
}
