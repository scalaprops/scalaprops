package scalaprops

import scalaz.Tag

object GenTags {

  sealed trait Num
  val Num = Tag.of[Num]

  sealed trait AlphaUpper
  val AlphaUpper = Tag.of[AlphaUpper]

  sealed trait AlphaLower
  val AlphaLower = Tag.of[AlphaLower]

  sealed trait Alpha
  val Alpha = Tag.of[Alpha]

  sealed trait AlphaNum
  val AlphaNum = Tag.of[AlphaNum]

  sealed trait Ascii
  val Ascii = Tag.of[Ascii]

}
