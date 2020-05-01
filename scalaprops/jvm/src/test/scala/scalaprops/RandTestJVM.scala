package scalaprops

import scala.util.Random

object RandTestJVM extends Scalaprops {
  private[this] def chooseLong(rng: Long => Rand) =
    Property.forAll(
      Iterator.fill(100000)((Random.nextLong, Random.nextLong, Random.nextLong)).forall {
        case (seed, y, z) =>
          val r = rng(seed).chooseLong(y, z)._2
          val min = math.min(y, z)
          val max = math.max(y, z)
          (min <= r) && (r <= max)
      }
    )

  val chooseLong32 = chooseLong(l => MersenneTwister32.fromSeed(l.toInt))
  val chooseLong64 = chooseLong(MersenneTwister64.standard)
}
