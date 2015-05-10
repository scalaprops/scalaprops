package scalaprops

import scalaz._
import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.std.option._

object OptionTTest extends Scalaprops{

  val iList = {
    type F[A] = OptionT[IList, A]
    Properties.either(
      "OptionT[IList, _]",
      scalazlaws.equal.all[OptionT[IList, Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  val maybe = {
    type F[A] = OptionT[Maybe, A]
    Properties.either(
      "OptionT[Maybe, _]",
      scalazlaws.equal.all[OptionT[Maybe, Int]],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

}
