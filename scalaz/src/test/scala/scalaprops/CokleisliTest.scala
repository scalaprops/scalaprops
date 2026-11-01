package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

object CokleisliTest extends Scalaprops {
  override val param: Param = Param.withCurrentTimeSeed().copy(maxSize = 30)

  private[this] val e = new FunctionEqual(3)

  implicit def cokleisliEqual[F[_], A, B: Equal](implicit F: Gen[F[A]]): Equal[Cokleisli[F, A, B]] = {
    import e.*
    Equal[F[A] => B].contramap(_.run)
  }

  property("maybe1") = {
    type C[A, B] = Cokleisli[Maybe, A, B]

    Properties.list(
      scalazlaws.compose.all[C],
      scalazlaws.profunctor.all[C]
    )
  }

  property("maybe2") = {
    type F[A] = Cokleisli[Maybe, Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

  property("nel1") = scalazlaws.arrow.all[({ type l[a, b] = Cokleisli[NonEmptyList, a, b] })#l]
  property("nel2") = {
    type F[A] = Cokleisli[NonEmptyList, Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

  property("tree1") = scalazlaws.arrow.all[({ type l[a, b] = Cokleisli[Tree, a, b] })#l]
  property("tree2") = {
    type F[A] = Cokleisli[Tree, Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

  property("zipper1") = scalazlaws.arrow.all[({ type l[a, b] = Cokleisli[Zipper, a, b] })#l]
  property("zipper2") = {
    type F[A] = Cokleisli[Zipper, Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }
}
