package scalaprops

import scalaprops.Property.forAllG

object StringTest extends Scalaprops {

  val genString = {
    val g = for {
      min <- Gen.choose(-10, 50)
      s <- Gen.genString(Gen.asciiChar, min)
    } yield (min, s)

    val p = forAllG(g) {
      case (min, s) => s.length >= min
    }
    p.toProperties("minimum length")
  }

  val nonEmptyString = forAllG(Gen.nonEmptyString(Gen.asciiChar))(_.nonEmpty)

}
