package scalaprops

abstract class Seed extends Product with Serializable {
  def createRand: Rand
}

object Seed {
  final case class LongSeed(value: Long) extends Seed {
    override def createRand: Rand = Platform.randFromLong(value)
  }
  final case class IntSeed(value: Int) extends Seed {
    override def createRand: Rand = Platform.randFromInt(value)
  }
  final case class RandSeed(value: Rand) extends Seed {
    override def createRand: Rand = value
  }
}
