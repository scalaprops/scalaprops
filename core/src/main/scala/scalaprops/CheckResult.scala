package scalaprops

import scalaz.IList

sealed abstract class CheckResult extends Product with Serializable{
  def succeeded: Int
  def discarded: Int
  def seed: Seed
}

object CheckResult {
  final case class Passed(override val succeeded: Int, override val discarded: Int, override val seed: Seed) extends CheckResult
  final case class Proven(override val succeeded: Int, override val discarded: Int, override val seed: Seed) extends CheckResult
  final case class Falsified(override val succeeded: Int, override val discarded: Int, args: IList[Arg], override val seed: Seed) extends CheckResult
  final case class Exhausted(override val succeeded: Int, override val discarded: Int, override val seed: Seed) extends CheckResult
  final case class PropException(override val succeeded: Int, override val discarded: Int, args: IList[Arg], exception: Throwable, override val seed: Seed) extends CheckResult
  final case class GenException(override val succeeded: Int, override val discarded: Int, exception: Throwable, override val seed: Seed) extends CheckResult
  final case class Timeout(override val succeeded: Int, override val discarded: Int, override val seed: Seed) extends CheckResult
  final case class Ignored(override val succeeded: Int, override val discarded: Int, reason: String, override val seed: Seed) extends CheckResult
}
