package scalaprops

import scalaz._

object GenTags {

  private[this] type CharInstance[A] =
    Monoid[A] with Enum[A] with Show[A]

  private[this] type StringInstance[A] =
    Monoid[A] with Show[A] with Order[A] with IsEmpty[({type l[_] = A})#l]

  sealed trait Num
  val Num = Tag.of[Num]

  implicit val numCharInstance: CharInstance[Char @@ Num] =
    Num.subst[CharInstance, Char](scalaz.std.anyVal.char)
  implicit val numStringInstance: StringInstance[String @@ Num] =
    Num.subst[StringInstance, String](scalaz.std.string.stringInstance)

  sealed trait AlphaUpper
  val AlphaUpper = Tag.of[AlphaUpper]

  implicit val alphaUpperCharInstance: CharInstance[Char @@ AlphaUpper] =
    AlphaUpper.subst[CharInstance, Char](scalaz.std.anyVal.char)
  implicit val alphaUpperStringInstance: StringInstance[String @@ AlphaUpper] =
    AlphaUpper.subst[StringInstance, String](scalaz.std.string.stringInstance)

  sealed trait AlphaLower
  val AlphaLower = Tag.of[AlphaLower]

  implicit val alphaLowerCharInstance: CharInstance[Char @@ AlphaLower] =
    AlphaLower.subst[CharInstance, Char](scalaz.std.anyVal.char)
  implicit val alphaLowerStringInstance: StringInstance[String @@ AlphaLower] =
    AlphaLower.subst[StringInstance, String](scalaz.std.string.stringInstance)

  sealed trait Alpha
  val Alpha = Tag.of[Alpha]

  implicit val alphaCharInstance: CharInstance[Char @@ Alpha] =
    Alpha.subst[CharInstance, Char](scalaz.std.anyVal.char)
  implicit val alphaStringInstance: StringInstance[String @@ Alpha] =
    Alpha.subst[StringInstance, String](scalaz.std.string.stringInstance)

  sealed trait AlphaNum
  val AlphaNum = Tag.of[AlphaNum]

  implicit val alphaNumCharInstance: CharInstance[Char @@ AlphaNum] =
    AlphaNum.subst[CharInstance, Char](scalaz.std.anyVal.char)
  implicit val alphaNumStringInstance: StringInstance[String @@ AlphaNum] =
    AlphaNum.subst[StringInstance, String](scalaz.std.string.stringInstance)

  sealed trait Ascii
  val Ascii = Tag.of[Ascii]

  implicit val asciiCharInstance: CharInstance[Char @@ Ascii] =
    Ascii.subst[CharInstance, Char](scalaz.std.anyVal.char)
  implicit val asciiStringInstance: StringInstance[String @@ Ascii] =
    Ascii.subst[StringInstance, String](scalaz.std.string.stringInstance)

}
