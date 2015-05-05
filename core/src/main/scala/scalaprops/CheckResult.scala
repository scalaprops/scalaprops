package scalaprops

import scalaz.IList

sealed abstract class CheckResult extends Product with Serializable{
  def succeeded: Int
  def discarded: Int
}

object CheckResult {
  final case class Passed(override val succeeded: Int, override val discarded: Int) extends CheckResult
  final case class Proven(override val succeeded: Int, override val discarded: Int, args: IList[Arg]) extends CheckResult
  final case class Falsified(override val succeeded: Int, override val discarded: Int, args: IList[Arg]) extends CheckResult
  final case class Exhausted(override val succeeded: Int, override val discarded: Int) extends CheckResult
  final case class PropException(override val succeeded: Int, override val discarded: Int, args: IList[Arg], exception: Throwable) extends CheckResult
  final case class GenException(override val succeeded: Int, override val discarded: Int, exception: Throwable) extends CheckResult
  final case class Timeout(override val succeeded: Int, override val discarded: Int) extends CheckResult
}
