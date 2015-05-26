package scalaprops

import scalaz._

abstract class Rand {

  def nextInt: (Int, Rand)

  def nextLong: (Long, Rand)

  protected[this] final def nextIntFromNextLong: (Int, Rand) = {
    val (n, r) = nextLong
    ((n >>> 32).toInt, r)
  }

  protected[this] final def nextLongFromNextInt: (Long, Rand) = {
    val (n1, _) = nextInt
    val (n2, r) = nextInt
    val x = ((n1 & 0xffffffffL) << 32) | (n2 & 0xffffffffL)
    (x, r)
  }

  def nextDouble: (Double, Rand) =
    Rand.nextDouble(this)

  def choose(from: Int, to: Int): (Int, Rand) = {
    if(from == to) {
      (from, this.nextInt._2)
    } else {
      val min = math.min(from, to)
      val max = math.max(from, to)
      @annotation.tailrec
      def loop(state: Rand): (Int, Rand) = {
        val next = state.nextInt
        if (min <= next._1 && next._1 <= max) {
          next
        } else if(0 < (max - min)){
          val x = (next._1 % (max - min + 1)) + min
          if (min <= x && x <= max) {
            x -> next._2
          } else {
            loop(next._2)
          }
        } else {
          loop(next._2)
        }
      }
      loop(this)
    }
  }

  def next: Rand =
    nextInt._2

  def reseed(newSeed: Long): Rand
}

object Rand{

  implicit val randGen: Gen[Rand] =
    Gen.gen{ (_, r) =>
      val next = r.next
      (next, next)
    }

  implicit val randCogen: Cogen[Rand] =
    new Cogen[Rand] {
      def cogen[B](a: Rand, g: Gen[B]): Gen[B] =
        Gen.gen((size, _) => g.f(size, a))
    }

  implicit val randEqual: Equal[Rand] =
    Equal.equalA[Rand]

  def listInt(size: Int, from: Int, to: Int, initialState: Rand): (Rand, List[Int]) =
    (1 to size).foldLeft((initialState, List.empty[Int])){
      case ((state, acc), _) =>
        val next = state.choose(from, to)
        next._2 -> (next._1 :: acc)
    }

  def standard(s: Long): Rand =
    fromSeed(s.toInt)

  private[scalaprops] final val defaultSeed = 5489

  def fromSeed(seed: Int = defaultSeed): Rand =
    MersenneTwister32.fromSeed(seed)

  // Generates a random Double in the interval [0, 1)
  def nextDouble(state: Rand): (Double, Rand) = {
    val x = state.nextInt
    val a: Long = (x._1.toLong & 0xffffffffL) >>> 5
    val y = x._2.nextInt
    val b: Long = (y._1.toLong & 0xffffffffL) >>> 6
    val r = (a * 67108864.0 + b) / 9007199254740992.0
    r -> y._2
  }
}
