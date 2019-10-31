package scalaprops

import scalaz.IList
import ScalapropsScalaz._

object CogenTest extends Scalaprops {
  val `Cogen[Array[Byte]] instance` = Property.forAll {
    (Cogen[Array[Byte]] != null) && (Cogen[Array[Byte]] eq Cogen[Array[Byte]])
  }

  val `Cogen[List[Byte]] instance` = Property.forAll {
    (Cogen[List[Byte]] != null) && (Cogen[List[Byte]] eq Cogen[List[Byte]])
  }

  val `Cogen[IList[Byte]] instance` = Property.forAll {
    (Cogen[IList[Byte]] != null) && (Cogen[IList[Byte]] eq Cogen[IList[Byte]])
  }
}
