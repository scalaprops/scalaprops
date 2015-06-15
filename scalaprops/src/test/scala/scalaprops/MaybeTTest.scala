package scalaprops

import scalaz._
import scalaz.std.anyVal._

object MaybeTTest extends Scalaprops {

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
