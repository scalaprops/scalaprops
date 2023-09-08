package scalaprops

import java.nio.ByteBuffer
import java.util.Arrays

final class MersenneTwister64 private (private val mt0: Array[Long], private val mti0: Int = 313) extends Rand { // N + 1 = 313

  import MersenneTwister64.{mag01, BYTES, LowerMask, M, M_1, M_N, N, N_1, N_M, UpperMask}

  override def equals(other: Any): Boolean =
    other match {
      case that: MersenneTwister64 => this === that
      case _ => false
    }

  override def hashCode = mti0

  def ===(that: MersenneTwister64): Boolean = {
    (this eq that) || ((this.mti0 == that.mti0) && Arrays.equals(this.mt0, that.mt0))
  }

  override def next: MersenneTwister64 =
    nextLong._1

  def getSeedBytes(): Array[Byte] = {
    val bytes = new Array[Byte](BYTES)
    val bb = ByteBuffer.wrap(bytes)

    var i = 0
    while (i < N) {
      bb.putLong(mt0(i))
      i += 1
    }
    bb.putInt(mti0)
    bytes
  }

  def setSeedBytes(bytes: Array[Byte]): MersenneTwister64 = {
    val mt = mt0.clone()
    val bs = if (bytes.length < BYTES) Arrays.copyOf(bytes, BYTES) else bytes
    val bb = ByteBuffer.wrap(bs)
    var i = 0
    while (i < N) {
      mt(i) = bb.getLong()
      i += 1
    }
    val mti = bb.getInt
    new MersenneTwister64(mt, mti)
  }

  // TODO improve
  def reseed(n: Long): MersenneTwister64 = next

  override def setIntSeed(seed: Int): MersenneTwister64 =
    MersenneTwister64.fromTime(seed)

  def nextLong: (MersenneTwister64, Long) = {
    var mti = mti0
    var x = 0L

    val mt1 = if (mti >= N) {
      val mt = mt0.clone()
      var kk = 0

      while (kk < N_M) {
        x = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (x >>> 1) ^ mag01(x)
        kk += 1
      }

      while (kk < N_1) {
        x = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M_N) ^ (x >>> 1) ^ mag01(x)
        kk += 1
      }

      x = (mt(N_1) & UpperMask) | (mt(0) & LowerMask)
      mt(N_1) = mt(M_1) ^ (x >>> 1) ^ mag01(x)

      mti = 0
      mt
    } else {
      mt0
    }

    x = mt1(mti)
    mti += 1

    // Tempering
    x ^= (x >>> 29) & 0x5555555555555555L
    x ^= (x << 17) & 0x71d67fffeda60000L
    x ^= (x << 37) & 0xfff7eee000000000L
    x ^= (x >>> 43)

    (new MersenneTwister64(mt1, mti), x)
  }

  override def nextInt: (Rand, Int) =
    nextIntFromNextLong

  override def toString: String = {
    mt0.mkString("MersenneTwister64(Array(", ",", s"), ${mti0})")
  }
}

object MersenneTwister64 {
  private final val UpperMask = 0xffffffff80000000L // = 0xFFFFFFFFFFFFFFFFL ^ Int.MinValue
  private final val LowerMask = 0x7fffffffL // = Int.MinValue

  private final val N = 312
  private final val M = 156

  private final val N_M = N - M
  private final val N_1 = N - 1

  private final val M_N = M - N
  private final val M_1 = M - 1

  private final val BYTES = N * 8 + 4

  @inline private def mag01(x: Long) =
    if ((x & 1) == 0) 0L else 0xb5026f5aa96619eL

  def fromSeed(mt: Array[Long], mti: Int): MersenneTwister64 = {
    assert(mt.length == N)
    new MersenneTwister64(mt, mti)
  }

  final val defaultSeed = 5489

  def standard(seed: Long) =
    new MersenneTwister64(seedFromLong(N, seed))

  def fromSeed(seed: Int = defaultSeed): MersenneTwister64 =
    fromTime(seed)

  def fromArray(arr: Array[Long]): MersenneTwister64 =
    fromSeed(seedFromArray(N, arr), N + 1)

  def fromBytes(bytes: Array[Byte]): MersenneTwister64 =
    fromArray(longsFromBytes(bytes, bytes.length / 8))

  def fromTime(time: Long = System.nanoTime): MersenneTwister64 =
    fromSeed(seedFromLong(N, time), N + 1)

  def seedFromInt(length: Int, seed: Int): Array[Int] = {
    val a = new Array[Int](length)
    a(0) = seed

    var i = 1
    while (i < length) {
      val x = a(i - 1)
      a(i) = 1812433253 * (x ^ (x >>> 30)) + i
      i += 1
    }

    a
  }

  private def seedFromLong(length: Int, seed: Long): Array[Long] = {
    val a = new Array[Long](length)
    a(0) = seed

    var i = 1
    while (i < length) {
      val x = a(i - 1)
      a(i) = 6364136223846793005L * (x ^ (x >>> 62)) + i
      i += 1
    }

    a
  }

  private def seedFromArray(length: Int, seed: Array[Long]): Array[Long] = {
    val a = seedFromLong(length, 19650218)
    val length_1 = length - 1

    var i = 1
    var j = 0
    var k = java.lang.Math.max(length, seed.length)

    while (k != 0) {
      val x = a(i - 1)
      a(i) = a(i) ^ ((x ^ (x >>> 62)) * 3935559000370003845L) + seed(j) + j
      i += 1
      j += 1

      if (i >= length) {
        a(0) = a(length_1)
        i = 1
      }

      if (j >= seed.length) {
        j = 0
      }
      k -= 1
    }

    k = length - 1
    while (k != 0) {
      val x = a(i - 1)
      a(i) = a(i) ^ ((x ^ (x >>> 62)) * 2862933555777941757L) - i
      i += 1

      if (i >= length) {
        a(0) = a(length_1)
        i = 1
      }

      k -= 1
    }

    a(0) = 1L << 63
    a
  }

  private[this] def longFromByteBuffer(bb: ByteBuffer): Long =
    if (bb.remaining >= 8) {
      bb.getLong()
    } else {
      var n = 0L
      while (bb.remaining > 0) n = (n << 8) | bb.get
      n
    }

  private def longsFromBytes(bytes: Array[Byte], n: Int): Array[Long] =
    longsFromByteBuffer(ByteBuffer.wrap(bytes), n)

  private[this] def longsFromByteBuffer(bb: ByteBuffer, n: Int): Array[Long] = {
    val out = new Array[Long](n)
    var i = 0
    while (i < n && bb.remaining >= 8) {
      out(i) = bb.getLong()
      i += 1
    }
    if (i < n && bb.remaining > 0) out(i) = longFromByteBuffer(bb)
    out
  }
}
