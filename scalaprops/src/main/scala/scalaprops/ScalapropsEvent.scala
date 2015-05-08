package scalaprops

import sbt.testing._
import scalaz.\&/

final case class ScalapropsEvent(
  fullyQualifiedName: String,
  fingerprint: Fingerprint,
  selector: Selector,
  status: Status,
  throwable: OptionalThrowable,
  duration: Long,
  result: Throwable \&/ CheckResult
) extends Event
