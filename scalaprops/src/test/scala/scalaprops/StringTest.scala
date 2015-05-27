package scalaprops

import scalaprops.Property.forAll
import scalaz._
import scalaprops.GenTags._

object StringTest extends Scalaprops {

  private[this] def test[A](values: Seq[Char])(implicit
    M: Monoid[String @@ A],
    G: Gen[String @@ A],
    S: Show[String @@ A],
    O: Order[String @@ A],
    I: IsEmpty[({type l[_] = String @@ A})#l]
  ) = {
    val expect = values.toSet
    val x = forAll{ str: (String @@ A) =>
      Tag.unwrap(str).forall(expect)
    }
    val y = Properties.list(
      scalazlaws.monoid.all[String @@ A],
      scalazlaws.order.all[String @@ A],
      scalazlaws.isEmpty.all[({type l[_] = String @@ A})#l]
    )
    x.toProperties(()).product(y)
  }

  val num = test[GenTags.Num]('0' to '9')
  val upper = test[GenTags.AlphaUpper]('A' to 'Z')
  val lower = test[GenTags.AlphaLower]('a' to 'z')
  val alpha = test[GenTags.Alpha](('a' to 'z') ++ ('A' to 'Z'))
  val alphaNum = test[GenTags.AlphaNum](('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'))

}
