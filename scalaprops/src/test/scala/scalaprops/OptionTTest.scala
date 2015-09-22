package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.option._

object OptionTTest extends Scalaprops{

  val disjunction = {
    type F[E, A] = OptionT[({type l[a] = E \/ a})#l, A]

    implicit def gen[G[_, _], E, A](implicit G: Gen[G[E, Option[A]]]) =
      Gen.optionTGen[({type l[a] = G[E, a]})#l, A]

    implicit def equal[G[_, _], E, A](implicit G: Equal[G[E, Option[A]]]) =
      OptionT.optionTEqual[({type l[a] = G[E, a]})#l, A]

    scalazlaws.monadError.all[F, Byte]
  }

  val iList = {
    type F[A] = OptionT[IList, A]
    Properties.list(
      scalazlaws.equal.all[OptionT[IList, Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  val maybe = {
    type F[A] = OptionT[Maybe, A]
    Properties.list(
      scalazlaws.equal.all[OptionT[Maybe, Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[OptionT]

}
