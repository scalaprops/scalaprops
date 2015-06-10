package scalaprops

import scalaz._

final class MersenneTwister32 private(private val array: Array[Long], private val mti: Int) extends Rand {
  private def newArray: Array[Long] = array.clone()

  def nextInt: (Rand, Int) =
    MersenneTwister32.nextInt(this)

  def nextLong: (Rand, Long) =
    nextLongFromNextInt

  override def equals(other: Any): Boolean =
    other match {
      case that: MersenneTwister32 => this.===(that)
      case _ => false
    }

  override def hashCode: Int = mti

  def ===(that: MersenneTwister32): Boolean =
    (this.mti == that.mti) && java.util.Arrays.equals(this.array, that.array)

  def reseed(newSeed: Long): MersenneTwister32 =
    MersenneTwister32.standard(newSeed)
}

object MersenneTwister32{
  private[this] val N = 624
  private[this] val M = 397

  private[this] val MatrixA = 0x9908b0dfL

  private[this] val UpperMask = 0x80000000L
  private[this] val LowerMask = 0x7fffffffL

  implicit val randEqual: Equal[MersenneTwister32] =
    Equal.equalA[MersenneTwister32]

  private def apply(array: Array[Long], mti: Int): MersenneTwister32 =
    new MersenneTwister32(array, mti)

  def standard(s: Long): MersenneTwister32 =
    fromSeed(s.toInt)

  private[scalaprops] val default: MersenneTwister32 = fromSeed0(Rand.defaultSeed)

  def fromSeed(seed: Int = Rand.defaultSeed): MersenneTwister32 = {
    if(seed == Rand.defaultSeed) {
      default
    } else {
      fromSeed0(seed)
    }
  }

  private[scalaprops] def fromSeed0(seed: Int): MersenneTwister32 = {
    val mt = new Array[Long](N)
    mt(0) = seed
    var i = 1
    while(i < N){
      mt(i) = (1812433253L * (mt(i - 1) ^ (mt(i - 1) >>> 30)) + i) & 0xffffffffL
      i += 1
    }
    MersenneTwister32(mt, N + 1)
  }

  def nextInt(state: MersenneTwister32): (MersenneTwister32, Int) = {
    var mti = state.mti
    var y = 0L

    val mt0 = if (mti >= N) {
      val mt = state.newArray
      val mag01 = Array(0L, MatrixA)

      var kk = 0
      while (kk < N - M) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
        kk += 1
      }
      while (kk < N - 1) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + (M - N)) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
        kk += 1
      }
      y = (mt(N - 1) & UpperMask) | (mt(0) & LowerMask)
      mt(N - 1) = mt(M - 1) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)

      mti = 0
      mt
    }else{
      state.array
    }

    y = mt0(mti); mti += 1
    y ^= y >>> 11
    y ^= (y << 7) & 0x9d2c5680L
    y ^= (y << 15) & 0xefc60000L
    y ^= (y >>> 18)

    (MersenneTwister32(mt0, mti), y.toInt)
  }

}
