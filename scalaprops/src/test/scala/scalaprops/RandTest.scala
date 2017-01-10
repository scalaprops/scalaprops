package scalaprops

import scala.util.Random
import scalaz.std.string._

object RandTest extends Scalaprops{
  val testEqualLaw = scalazlaws.equal.all[Rand]

  val testRandChoose: Property = Property.forAll(
    List.fill(10000)((Random.nextLong, Random.nextInt, Random.nextInt)).forall {
      case (seed, y, z) =>
        val r = Rand.standard(seed).choose(y, z)._2
        val min = math.min(y, z)
        val max = math.max(y, z)
        (min <= r) && (r <= max)
    }
  )

  val chooseNotBias = {
    val N = 10
    val size = 10000

    def test[A](g: Gen[A], seed: Long, count: Int) = {
      val x = g.samples(listSize = size, seed = seed).groupBy(identity).map{
        case (k, v) => k -> v.size
      }.toList.sortBy(_._2)
      val min = (size / count) * 0.8
      val max = (size / count) * 1.3
      x.foreach{ case (_, a) =>
        assert(min < a && a < max, x)
      }
      true
    }

    Properties.list(
      Property.forAllG(
        Gen.choose(Int.MinValue, Int.MaxValue - N),
        Gen.choose(1, N),
        Gen[Long]
      ){ (n, m, seed) =>
        test(Gen.choose(n, n + m), seed, m + 1)
      }.toProperties("choose"),
      Property.forAllG(
        Gen.chooseLong(Long.MinValue, Long.MaxValue - N),
        Gen.choose(1, N),
        Gen[Long]
      ){ (n, m, seed) =>
        test(Gen.chooseLong(n, n + m), seed, m + 1)
      }.toProperties("chooseLong")
    )
  }

  val chooseLong1 = Property.forAllG(Gen[Rand], Gen[Long], Gen[Long]){ (rng, y, z) =>
    val r = rng.chooseLong(y, z)._2
    val min = math.min(y, z)
    val max = math.max(y, z)
    (min <= r) && (r <= max)
  }.toProperties((), Param.minSuccessful(10000))

  val chooseLong2 = Property.forAllG(Gen[Long], Gen.choose(0, 30), Gen[Long]){ (a, b, seed) =>
    val c = a + b
    val max = if(c >= a) c else Long.MaxValue
    val x = Gen.chooseLong(a, max).samples(listSize = 1000, seed = seed).distinct.size
    x == (max - a + 1)
  }

  val issue28 = Property.forAll {
    Gen.chooseLong(0, Int.MaxValue.toLong * 2 + 1).sample()
    true
  }

  val chooseLong3 = Property.forAll{ seed: Long =>
    def boundaries(x: Int): List[Long] = {
      val a: Long = List.fill(x)(2L).product
      List(- a - 1, - a, - a + 1, 0, a - 1, a, a + 1)
    }

    for {
      x <- 1 to 63
      y <- 1 to 63
      m <- boundaries(x)
      n <- boundaries(y)
      max = math.max(m, n)
      min = math.min(m, n)
      size = 3
    } {
      Gen.chooseLong(m, n).samples(seed = seed, listSize = size).foreach{ a =>
        assert(min <= a && a <= max, s"$seed $a $min $max")
      }

      if(m.isValidInt && n.isValidInt) {
        Gen.choose(m.toInt, n.toInt).samples(seed = seed, listSize = size).foreach{ a =>
          assert(min <= a && a <= max, s"$seed $a $min $max")
        }
      }
    }
    true
  }.toProperties((), Param.minSuccessful(3))
}
