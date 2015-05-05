package scalaprops

import sbt.testing.{Fingerprint, Framework}

class ScalapropsFramework extends Framework{
  override def name() = "Scalaprops"

  override def fingerprints() =
    Array[Fingerprint](ScalapropsFingerprint)

  override def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader) =
    new ScalapropsRunner(args, remoteArgs, testClassLoader)
}
