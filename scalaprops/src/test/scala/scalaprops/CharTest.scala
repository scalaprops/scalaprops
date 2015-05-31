package scalaprops

import scalaprops.Property.forAll
import scalaz._
import scalaz.std.string._
import scalaprops.GenTags._

object CharTest extends Scalaprops {

  private[this] def checkValues[A](expect: Seq[Char])(implicit A: Gen[Char @@ A]) = {
    val set = expect.toSet
    forAll{ c: (Char @@ A) =>
      set.contains(Tag.unwrap(c))
    }
  }

  private[this] def checkSize[A](size: Int)(implicit A: Gen[Char @@ A]) = forAll{
    Macros.assertEqual(A.samples(listSize = 500).distinct.size, size)
  }

  private[this] def check[A](expect: Seq[Char])(implicit
    A: Gen[Char @@ A],
    C: Cogen[Char @@ A],
    M: Monoid[Char @@ A],
    E: Enum[Char @@ A],
    S: Show[Char @@ A]
  ) = {
    val x = Properties.list(
      checkValues[A](expect).toProperties("values"),
      checkSize[A](expect.distinct.size).toProperties("size")
    )

    val y = Properties.list(
      scalazlaws.enum.all[Char @@ A],
      scalazlaws.monoid.all[Char @@ A]
    )

    x.product(y)
  }

  val num = check[GenTags.Num]('0' to '9')
  val upper = check[GenTags.AlphaUpper]('A' to 'Z')
  val lower = check[GenTags.AlphaLower]('a' to 'z')
  val alpha = check[GenTags.Alpha](('a' to 'z') ++ ('A' to 'Z'))
  val alphaNum = check[GenTags.AlphaNum](('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))

}
