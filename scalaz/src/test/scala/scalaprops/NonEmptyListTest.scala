package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object NonEmptyListTest extends Scalaprops {
  val testLaws = Properties
    .list(
      scalazlaws.traverse1.all[NonEmptyList],
      scalazlaws.monad.all[NonEmptyList],
      scalazlaws.zip.all[NonEmptyList],
      scalazlaws.align.all[NonEmptyList],
      scalazlaws.comonad.all[NonEmptyList],
      scalazlaws.bindRec.all[NonEmptyList],
      scalazlaws.plus.all[NonEmptyList]
    )
    .andThenParamPF { case Or.R(Or.L(ScalazLaw.bindRecTailrecBindConsistency)) =>
      Param.maxSize(20) andThen Param.minSuccessful(10)
    }

  val size = Property.forAllG(Gen.positiveByte, Gen[Long]) { (s, seed) =>
    Gen[NonEmptyList[Unit]].sample(s, seed).size <= s
  }
}
