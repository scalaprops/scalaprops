package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object EitherTTest extends Scalaprops {
  val iListBindRec =
    scalazlaws.bindRec.laws[({ type l[a] = EitherT[Byte, IList, a] })#l].andThenParam(Param.maxSize(1))

  val maybe = {
    type F[A] = EitherT[Int, Maybe, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[Int, Maybe, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  val maybe2 =
    scalazlaws.bitraverse.all[({ type l[a, b] = EitherT[a, Maybe, b] })#l]

  val iList = {
    type F[A] = EitherT[Int, IList, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[Int, IList, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  val nel = {
    type F[A] = EitherT[Int, NonEmptyList, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[Int, NonEmptyList, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({ type l[f[_], a] = EitherT[Int, f, a] })#l]
}
