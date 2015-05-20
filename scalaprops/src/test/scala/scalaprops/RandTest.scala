package scalaprops

import scala.util.Random

object RandTest extends Scalaprops{
  val testEqualLaw = scalazlaws.equal.all[Rand]

  val testRandChoose: Property = Property.forAll(
    List.fill(10000)((Random.nextLong, Random.nextInt, Random.nextInt)).forall {
      case (seed, y, z) =>
        val r = Rand.standard(seed).choose(y, z)._1
        val min = math.min(y, z)
        val max = math.max(y, z)
        (min <= r) && (r <= max)
    }
  )
}
