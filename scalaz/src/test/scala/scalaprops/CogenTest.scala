package scalaprops

import ScalapropsScalaz.*
import scalaz.IList

object CogenTest extends Scalaprops {
  property("Cogen[Array[Byte]] instance") = Property.forAll {
    (Cogen[Array[Byte]] != null) && (Cogen[Array[Byte]] eq Cogen[Array[Byte]])
  }

  property("Cogen[List[Byte]] instance") = Property.forAll {
    (Cogen[List[Byte]] != null) && (Cogen[List[Byte]] eq Cogen[List[Byte]])
  }

  property("Cogen[IList[Byte]] instance") = Property.forAll {
    (Cogen[IList[Byte]] != null) && (Cogen[IList[Byte]] eq Cogen[IList[Byte]])
  }
}
