package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object EitherTTest extends Scalaprops {
  property("iListBindRec") =
    scalazlaws.bindRec.laws[({ type l[a] = EitherT[Byte, IList, a] })#l].andThenParam(Param.maxSize(1))

  property("maybe") = {
    type F[A] = EitherT[Int, Maybe, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[Int, Maybe, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  property("maybe2") = scalazlaws.bitraverse.all[({ type l[a, b] = EitherT[a, Maybe, b] })#l]

  property("iList") = {
    type F[A] = EitherT[Int, IList, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[Int, IList, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  property("nel") = {
    type F[A] = EitherT[Int, NonEmptyList, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[Int, NonEmptyList, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  property("monadTrans") = scalazlaws.monadTrans.all[({ type l[f[_], a] = EitherT[Int, f, a] })#l]
}
