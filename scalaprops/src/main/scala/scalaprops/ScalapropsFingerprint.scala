package scalaprops

import sbt.testing.SubclassFingerprint

private[scalaprops] object ScalapropsFingerprint extends SubclassFingerprint {
  override def isModule(): Boolean = true
  override def superclassName(): String = "scalaprops.Scalaprops"
  override def requireNoArgConstructor(): Boolean = true
}
