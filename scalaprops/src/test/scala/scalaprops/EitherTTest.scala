package scalaprops

import scalaz._
import scalaz.std.anyVal._

object EitherTTest extends Scalaprops {

  val testEitherTMaybe =
    Properties.list(
      scalazlaws.equal.all[EitherT[Maybe, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[Maybe, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[Maybe, Int, a]})#l]
    )

  val testEitherTMaybe2 =
    scalazlaws.bitraverse.all[({type l[a, b] = EitherT[Maybe, a, b]})#l]

  val testEitherTIList =
    Properties.list(
      scalazlaws.equal.all[EitherT[IList, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[IList, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[IList, Int, a]})#l]
    )

  val testEitherTNel =
    Properties.list(
      scalazlaws.equal.all[EitherT[NonEmptyList, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l]
    )

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = EitherT[f, Int, a]})#l]

}
