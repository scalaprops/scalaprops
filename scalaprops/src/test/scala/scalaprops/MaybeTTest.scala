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

}
