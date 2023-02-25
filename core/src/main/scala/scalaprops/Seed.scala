package scalaprops

import scala.runtime.AbstractFunction1

abstract class Seed extends Product with Serializable {
  def createRand: Rand
}

object Seed {
  final case class LongSeed(value: Long) extends Seed {
    override def createRand: Rand = Platform.randFromLong(value)
  }
  object LongSeed extends AbstractFunction1[Long, LongSeed]
  final case class IntSeed(value: Int) extends Seed {
    override def createRand: Rand = Platform.randFromInt(value)
  }
  object IntSeed extends AbstractFunction1[Int, IntSeed]
  final case class RandSeed(value: Rand) extends Seed {
    override def createRand: Rand = value
  }
  object RandSeed extends AbstractFunction1[Rand, RandSeed]
}
