package scalaprops

import CheckResultError._

sealed abstract class CheckResultError extends Product with Serializable {
  def value: Option[CheckResult] = this match {
    case Value(a) =>
      Some(a)
    case Both(_, a) =>
      Some(a)
    case Err(_) =>
      None
  }

  def error: Option[Throwable] = this match {
    case Value(_) =>
      None
    case Both(e, _) =>
      Some(e)
    case Err(e) =>
      Some(e)
  }
}

object CheckResultError {
  final case class Value(r: CheckResult) extends CheckResultError
  final case class Both(err: Throwable, r: CheckResult) extends CheckResultError
  final case class Err(err: Throwable) extends CheckResultError
}
