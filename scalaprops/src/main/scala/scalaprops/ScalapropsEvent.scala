package scalaprops

import sbt.testing._

final case class ScalapropsEvent(
  fullyQualifiedName: String,
  fingerprint: Fingerprint,
  selector: Selector,
  status: Status,
  throwable: OptionalThrowable,
  duration: Long
) extends Event
