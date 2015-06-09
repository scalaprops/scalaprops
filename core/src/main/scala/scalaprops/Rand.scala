package scalaprops

import scalaz._

abstract class Rand {

  def nextInt: (Rand, Int)

  def nextLong: (Rand, Long)

  protected[this] final def nextIntFromNextLong: (Rand, Int) = {
    val (r, n) = nextLong
    (r, (n >>> 32).toInt)
  }

  protected[this] final def nextLongFromNextInt: (Rand, Long) = {
    val (_, n1) = nextInt
    val (r, n2) = nextInt
    val x = ((n1 & 0xffffffffL) << 32) | (n2 & 0xffffffffL)
    (r, x)
  }

  def nextDouble: (Rand, Double) =
    Rand.nextDouble(this)

  def chooseLong(from: Long, to: Long): (Rand, Long) = {
    if(from == to) {
      (this.nextInt._1, from)
    } else {
      val min = math.min(from, to)
      val max = math.max(from, to)
      @annotation.tailrec
      def loop(state: Rand): (Rand, Long) = {
        val next = state.nextLong
        if (min <= next._2 && next._2 <= max) {
          next
        } else if(0 < (max - min)){
          val x = (next._2 % (max - min + 1)) + min
          if (min <= x && x <= max) {
            (next._1, x)
          } else {
            loop(next._1)
          }
        } else {
          loop(next._1)
        }
      }
      loop(this)
    }
  }

  def choose(from: Int, to: Int): (Rand, Int) = {
    if(from == to) {
      (this.nextInt._1, from)
    } else {
      val min = math.min(from, to)
      val max = math.max(from, to)
      @annotation.tailrec
      def loop(state: Rand): (Rand, Int) = {
        val next = state.nextInt
        if (min <= next._2 && next._2 <= max) {
          next
        } else if(0 < (max - min)){
          val x = (next._2 % (max - min + 1)) + min
          if (min <= x && x <= max) {
            (next._1, x)
          } else {
            loop(next._1)
          }
        } else {
          loop(next._1)
        }
      }
      loop(this)
    }
  }

  def next: Rand =
    nextInt._1

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
      def cogen[B](a: Rand, g: CogenState[B]) =
        CogenState(g.rand.next, Gen.gen((size, _) => g.gen.f(size, a)))
    }

  implicit val randEqual: Equal[Rand] =
    Equal.equalA[Rand]

  def standard(s: Long): Rand =
    fromSeed(s.toInt)

  private[scalaprops] final val defaultSeed = 5489

  def fromSeed(seed: Int = defaultSeed): Rand =
    MersenneTwister32.fromSeed(seed)

  // Generates a random Double in the interval [0, 1)
  def nextDouble(state: Rand): (Rand, Double) = {
    val x = state.nextInt
    val a: Long = (x._2.toLong & 0xffffffffL) >>> 5
    val y = x._1.nextInt
    val b: Long = (y._2.toLong & 0xffffffffL) >>> 6
    val r = (a * 67108864.0 + b) / 9007199254740992.0
    (y._1, r)
  }
}
