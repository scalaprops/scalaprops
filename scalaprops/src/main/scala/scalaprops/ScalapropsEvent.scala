package scalaprops

import sbt.testing.*

final class ScalapropsEvent(
  fullyQualifiedName0: String,
  fingerprint0: Fingerprint,
  selector0: Selector,
  status0: Status,
  throwable0: OptionalThrowable,
  duration0: Long,
  result0: CheckResultError
) extends Event {
  def fullyQualifiedName(): String = fullyQualifiedName0
  def fingerprint(): Fingerprint = fingerprint0
  def selector(): Selector = selector0
  def status(): Status = status0
  def throwable(): OptionalThrowable = throwable0
  def duration(): Long = duration0
  def result(): CheckResultError = result0
}
