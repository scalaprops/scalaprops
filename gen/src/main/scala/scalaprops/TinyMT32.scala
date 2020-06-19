package scalaprops

import java.math.BigInteger

import scala.annotation.tailrec

/**
 * TinyMT is a pseudo random number generator.
 *
 * <p>
 * To get an instance, call `TinyMT32.getDefault`
 * </p>
 * <p>
 * This class supports jump function. User can get an array of pseudo random
 * number generators by calling `TinyMT32#getDefaultArray`
 * </p>
 *
 * @author M. Saito
 * @see [[http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/TINYMT/index.html TinyMT web page]]
 */
object TinyMT32 {
  private final case class MutableState(
    var st0: Int,
    var st1: Int,
    var st2: Int,
    var st3: Int,
    var param: TinyMT32Parameter
  ) {
    def asImmutable: TinyMT32 =
      new TinyMT32(
        st0 = st0,
        st1 = st1,
        st2 = st2,
        st3 = st3,
        parameter = param
      )

    def next(): this.type = {
      var x = 0
      var y = 0
      y = st3
      x = (st0 & TinyMT32.MASK) ^ st1 ^ st2
      x ^= (x << TinyMT32.SH0)
      y ^= (y >>> TinyMT32.SH0) ^ x
      st0 = st1
      st3 = y
      st1 = st2 ^ param.getMat1(y)
      st2 = (x ^ (y << TinyMT32.SH1)) ^ param.getMat2(y)
      this
    }
  }

  /** bit size of int. */
  private val INT_SIZE: Int = 32

  /** least long over int. */
  private val LONG_LIMIT: Long = 0x100000000L

  /** initialize shift. */
  private val INITIALIZE_SHIFT: Int = 27

  /** initialize shift. */
  private val INITIALIZE_SHIFT2: Int = 30

  /** magic number. */
  private val MAGIC_NUMBER1: Int = 1664525

  /** magic number. */
  private val MAGIC_NUMBER2: Int = 1566083941

  /** magic number. */
  private val MAGIC_NUMBER3: Int = 1812433253

  /** int to float shift. */
  private val INT_TO_FLOAT_SHIFT: Int = 9

  /** long to double shift. */
  private val LONG_TO_DOUBLE_SHIFT: Int = 12

  /** hexadecimal base. */
  private val HEXA_DECIMAL_BASE: Int = 16

  /** int to unsigned long mask. */
  private val INT_TO_LONG_MASK: Long = 0xffffffffL

  /** long to double mask. */
  private val LONG_TO_DOUBLE_MASK: Long = 0x3ff0000000000000L

  /**
   * basic jump step.
   * every jump step is a multiple of this step.
   */
  private val BASIC_JUMP_STEP: BigInteger = new BigInteger("2").pow(64)

  /** mask pattern to limit internal size. */
  private val MASK: Int = 0x7fffffff

  /** fixed shift 0. */
  private val SH0: Int = 1

  /** fixed shift 1. */
  private val SH1: Int = 10

  /** fixed 8 bit shift. */
  private val SH8: Int = 8

  /** pre loop before generation. */
  private val MIN_LOOP: Int = 8

  /**
   * Factory method which returns the TinyMT with the first generated
   * parameter of TinyMTDC.
   *
   * @param seed
   * seed of pseudo random numbers.
   * @return TinyMT with the first parameter.
   */
  def getDefault(seed: String): TinyMT32 = {
    val defaultParameter = TinyMT32Parameter.getDefaultParameter
    (new TinyMT32(defaultParameter)).setSeed(seed)
  }

  /**
   * Factory method which returns the TinyMT with the first generated
   * parameter of TinyMTDC.
   *
   * @param seed
   * seed of pseudo random numbers.
   * @return TinyMT with the first parameter.
   */
  def getDefault(seed: Long): TinyMT32 = {
    val defaultParameter = TinyMT32Parameter.getDefaultParameter
    (new TinyMT32(defaultParameter)).setLongSeed(seed)
  }

  /**
   * get default TinyMT32 with seeding by array.
   *
   * @param seeds seeds for initialization.
   * @return random number generator TinyMT32
   */
  def getDefault(seeds: Array[Int]): TinyMT32 = {
    val defaultParameter = TinyMT32Parameter.getDefaultParameter
    new TinyMT32(defaultParameter).setSeed(seeds)
  }

  /**
   * Factory method which returns the TinyMT with the first generated
   * parameter of TinyMTDC. `System#nanoTime` and
   * `Thread#getId()` are used for seed.
   *
   * @return TinyMT with the first parameter.
   */
  def getDefault(): TinyMT32 = {
    val seed = new Array[Int](4)
    val time: Long = System.nanoTime
    val threadId: Long = Thread.currentThread.getId
    seed(0) = (time >>> INT_SIZE).toInt
    seed(1) = time.toInt
    seed(2) = (threadId >>> INT_SIZE).toInt
    seed(3) = threadId.toInt
    val defaultParameter = TinyMT32Parameter.getDefaultParameter
    new TinyMT32(defaultParameter).setSeed(seed)
  }

  /**
   * make and return an array of TinyMT. Each element has the same
   * characteristic polynomial with TinyMT gotten by getDefaultMT. Especially,
   * the first element is just same as default TinyMT. The second element has
   * the state of <b>jump</b> * 2<sup>64</sup> steps after the first element.
   * In other word, the first element will generate the same sequence with the
   * second element, after <b>jump</b> * 2<sup>64</sup> pseudo random number
   * generation.
   *
   * @param count
   * number of TinyMT to be created.
   * @param seed
   * seed of first element
   * @param jump
   * step is jump * 2<sup>64</sup>
   * @return array of TinyMT
   */
  def getDefaultArray(count: Int, seed: Long, jump: Long): Array[TinyMT32] = {
    val tiny: TinyMT32 = getDefault(seed)
    tiny.getJumpedArray(count, jump)
  }

  /**
   * Make and return an array of TinyMT. Each element has the same
   * characteristic polynomial with TinyMT gotten by getDefaultMT. Especially,
   * the first element is just same as default TinyMT. The second element has
   * the state of <b>jump</b> * 2<sup>64</sup> steps after the first element.
   * In other word, the first element will generate the same sequence with the
   * second element, after <b>jump</b> * 2<sup>64</sup> pseudo random number
   * generation.
   *
   * This is equals to TinyMT32.getDefault(seed).getJumpedArray(count, jump);
   *
   * @param count
   * number of TinyMT to be created.
   * @param seed
   * seed of first element
   * @param jump
   * step is jump * 2<sup>64</sup>
   * @return array of TinyMT
   */
  def getDefaultArray(count: Int, seed: String, jump: Long): Array[TinyMT32] = {
    val tiny = getDefault(seed)
    tiny.getJumpedArray(count, jump)
  }

  /**
   * return TinyMT32 instance whose parameter has ID = 1.
   *
   * @param threadId thread ID
   * @return TinyMT32 instance
   */
  def getThreadLocal(threadId: Long): TinyMT32 = {
    new TinyMT32(TinyMT32Parameter.getThreadLocalParameter(threadId))
  }
}

/**
 * @param parameter parameters for this generator.
 */
final case class TinyMT32(
  private val st0: Int,
  private val st1: Int,
  private val st2: Int,
  private val st3: Int,
  private val parameter: TinyMT32Parameter
) extends Rand {
  private def asMutable: TinyMT32.MutableState =
    TinyMT32.MutableState(
      st0 = st0,
      st1 = st1,
      st2 = st2,
      st3 = st3,
      param = parameter
    )

  /**
   * Constructor from a parameter.
   *
   * @param param
   * a parameter generated by TinyMTDC
   */
  private def this(param: TinyMT32Parameter) = {
    this(
      st0 = 0,
      st1 = 0,
      st2 = 0,
      st3 = 0,
      parameter = param
    )
  }

  def nextInt: (TinyMT32, Int) = {
    val s = nextState
    (s, s.output)
  }

  override def nextLong: (TinyMT32, Long) = {
    val (s0, x0) = nextInt
    val t: Long = (x0: Long) << TinyMT32.INT_SIZE
    val (s1, x1) = s0.nextInt
    val x2 = t | (x1 & TinyMT32.INT_TO_LONG_MASK)
    (s1, x2)
  }

  def reseed(seed: Long): TinyMT32 =
    setLongSeed(seed)

  def setLongSeed(seed: Long): TinyMT32 = {
    if ((seed >= 0) && (seed < TinyMT32.LONG_LIMIT)) {
      setIntSeed(seed.toInt)
    } else {
      val tmp = new Array[Int](2)
      tmp(0) = (seed & 0xffffffff).toInt
      tmp(1) = (seed >>> TinyMT32.INT_SIZE).toInt
      setSeed(tmp)
    }
  }

  def setSeed(seed: String): TinyMT32 = {
    val intSeeds = new Array[Int](seed.length)
    @tailrec def loop(i: Int): Unit = {
      if (i < intSeeds.length) {
        intSeeds(i) = seed.charAt(i)
        loop(i + 1)
      }
    }
    loop(0)
    setSeed(intSeeds)
  }

  def setSeed(seeds: Array[Int]): TinyMT32 = {
    val lag: Int = 1
    val mid: Int = 1
    val size: Int = 4
    var i: Int = 0
    var j: Int = 0
    var count: Int = 0
    var r: Int = 0
    val keyLength: Int = seeds.length
    val status = Array[Int](0, parameter.mat1, parameter.mat2, parameter.tmat)
    if (keyLength + 1 > TinyMT32.MIN_LOOP) {
      count = keyLength + 1
    } else {
      count = TinyMT32.MIN_LOOP
    }
    r = iniFunc1(status(0) ^ status(mid % size) ^ status((size - 1) % size))
    status(mid % size) += r
    r += keyLength
    status((mid + lag) % size) += r
    status(0) = r
    count -= 1

    {
      i = 1
      j = 0
      while ((j < count) && (j < keyLength)) {
        r = iniFunc1(status(i % size) ^ status((i + mid) % size) ^ status((i + size - 1) % size))
        status((i + mid) % size) += r
        r += seeds(j) + i
        status((i + mid + lag) % size) += r
        status(i % size) = r
        i = (i + 1) % size
        j += 1
      }
    }

    while (j < count) {
      r = iniFunc1(status(i % size) ^ status((i + mid) % size) ^ status((i + size - 1) % size))
      status((i + mid) % size) += r
      r += i
      status((i + mid + lag) % size) += r
      status(i % size) = r
      i = (i + 1) % size
      j += 1
    }

    {
      j = 0
      while (j < size) {
        r = iniFunc2(status(i % size) + status((i + mid) % size) + status((i + size - 1) % size))
        status((i + mid) % size) ^= r
        r -= i
        status((i + mid + lag) % size) ^= r
        status(i % size) = r
        i = (i + 1) % size
        j += 1
      }
    }

    val x = new TinyMT32(
      st0 = status(0),
      st1 = status(1),
      st2 = status(2),
      st3 = status(3),
      parameter = this.parameter
    ).periodCertification0

    @tailrec
    def loop(z: TinyMT32.MutableState, i: Int): TinyMT32 =
      if (i < TinyMT32.MIN_LOOP) {
        loop(z.next(), i + 1)
      } else {
        z.asImmutable
      }

    loop(x.asMutable, 0)
  }

  /**
   * sub function of initialization.
   *
   * @param x input number
   * @return scrambled integer
   */
  private def iniFunc1(x: Int): Int =
    (x ^ (x >>> TinyMT32.INITIALIZE_SHIFT)) * TinyMT32.MAGIC_NUMBER1

  /**
   * sub function of initialization.
   *
   * @param x input number
   * @return scrambled integer
   */
  private def iniFunc2(x: Int): Int =
    (x ^ (x >>> TinyMT32.INITIALIZE_SHIFT)) * TinyMT32.MAGIC_NUMBER2

  def setIntSeed(seed: Int): TinyMT32 = {
    val counterMask = 3
    val status = new Array[Int](4)
    status(0) = seed
    status(1) = parameter.mat1
    status(2) = parameter.mat2
    status(3) = parameter.tmat

    {
      @tailrec
      def loop0(i: Int): Unit = {
        if (i < TinyMT32.MIN_LOOP) {
          status(i & counterMask) ^= i + TinyMT32.MAGIC_NUMBER3 * (status((i - 1) & counterMask) ^ (status(
            (i - 1) & counterMask
          ) >>> TinyMT32.INITIALIZE_SHIFT2))
          loop0(i + 1)
        }
      }
      loop0(1)
    }

    val x = new TinyMT32(
      st0 = status(0),
      st1 = status(1),
      st2 = status(2),
      st3 = status(3),
      parameter = this.parameter
    ).periodCertification0

    @tailrec
    def loop(i: Int, z: TinyMT32.MutableState): TinyMT32 =
      if (i < TinyMT32.MIN_LOOP) {
        loop(i + 1, z.next())
      } else {
        z.asImmutable
      }

    loop(0, x.asMutable)
  }

  private def periodCertification0: TinyMT32 = {
    if (((st0 & TinyMT32.MASK) == 0) && (st1 == 0) && (st2 == 0) && (st3 == 0)) {
      new TinyMT32(
        st0 = 'T',
        st1 = 'I',
        st2 = 'N',
        st3 = 'Y',
        parameter = this.parameter
      )
    } else this
  }

  /** The state transition function. This function is F<sub>2</sub>-linear. */
  private def nextState: TinyMT32 = {
    var x = 0
    var y = 0
    y = st3
    x = (st0 & TinyMT32.MASK) ^ st1 ^ st2
    x ^= (x << TinyMT32.SH0)
    y ^= (y >>> TinyMT32.SH0) ^ x
    val x0 = st1
    val x3 = y
    val x1 = st2 ^ parameter.getMat1(y)
    val x2 = (x ^ (y << TinyMT32.SH1)) ^ parameter.getMat2(y)
    new TinyMT32(
      st0 = x0,
      st1 = x1,
      st2 = x2,
      st3 = x3,
      parameter = this.parameter
    )
  }

  /**
   * The output function.
   *
   * @return pseudo random number
   */
  private def output: Int = {
    var t0: Int = 0
    var t1: Int = 0
    t0 = st3
    t1 = st0 + (st2 >>> TinyMT32.SH8)
    t0 ^= t1
    t0 ^= parameter.getTmat(t1)
    t0
  }

  /**
   * make float random.
   *
   * @return float output.
   */
  private def outputFloat: Float = {
    var t0: Int = 0
    var t1: Int = 0
    t0 = st3
    t1 = st0 + (st2 >>> TinyMT32.SH8)
    t0 ^= t1
    t0 = (t0 >>> TinyMT32.INT_TO_FLOAT_SHIFT) ^ parameter.getTmatFloat(t1)
    java.lang.Float.intBitsToFloat(t0) - 1.0f
  }

  private def add0(that: TinyMT32): TinyMT32 =
    new TinyMT32(
      st0 = this.st0 ^ that.st0,
      st1 = this.st1 ^ that.st1,
      st2 = this.st2 ^ that.st2,
      st3 = this.st3 ^ that.st3,
      parameter = this.parameter
    )

  /**
   * jump function.
   *
   * @param pol jump polynomial
   * @return jumped new TinyMT
   */
  private def jump(pol: F2Polynomial): TinyMT32 = {
    val degree = pol.degree

    // TODO avoid allocate new instance each loop
    @tailrec
    def loop(i: Int, src: TinyMT32, that: TinyMT32): TinyMT32 =
      if (i > degree) {
        that
      } else if (pol.getCoefficient(i) == 1) {
        loop(i + 1, src.nextState, that.add0(src))
      } else {
        loop(i + 1, src.nextState, that)
      }

    loop(0, this, new TinyMT32(this.parameter))
  }

  /**
   * Make and return an array of TinyMT. Each element of the array has the
   * same characteristic polynomial with this. Especially, the first element
   * is just same as this. The second element has the state of <b>jump</b> *
   * 2<sup>64</sup> steps after the first element. In other word, the first
   * element will generate the same sequence with the second element, after
   * <b>jump</b> * 2<sup>64</sup> pseudo random number generation.
   *
   * <p>
   * Note: Do not call any setSeed methods after jump. Seeding will cancel
   * the effect of jump.
   * </p>
   *
   * @param count number of arrays
   * @param jump  jump step
   * @return jumped array of TinyMT32.
   */
  def getJumpedArray(count: Int, jump: Long): Array[TinyMT32] = {
    val tiny = new Array[TinyMT32](count)
    tiny(0) = this
    val poly = tiny(0).parameter.characteristic
    val pow = TinyMT32.BASIC_JUMP_STEP.multiply(BigInteger.valueOf(jump))
    val jumpPoly = F2Polynomial.X.powerMod(pow, poly)
    @tailrec def loop(i: Int): Unit = {
      if (i < count) {
        tiny(i) = tiny(i - 1).jump(jumpPoly)
        loop(i + 1)
      }
    }
    loop(1)
    tiny
  }

  override def nextDouble: (TinyMT32, Double) = {
    val temp = nextLong
    val x: Long = (temp._2 >>> TinyMT32.LONG_TO_DOUBLE_SHIFT) | TinyMT32.LONG_TO_DOUBLE_MASK
    (temp._1, java.lang.Double.longBitsToDouble(x) - 1.0)
  }

  def nextFloat: (TinyMT32, Float) = {
    val s = nextState
    (s, s.outputFloat)
  }

  /** return characteristic polynomial in hexadecimal format. */
  def getCharacteristic: String =
    parameter.characteristic.toString(TinyMT32.HEXA_DECIMAL_BASE)

  /** return ID of TinyMT. ID is not unique in TinyMT. */
  def getId: Int = parameter.id

  /** return Delta of TinyMT. */
  def getDelta: Int = parameter.delta

  /** return Hamming weight of characteristic polynomial of TinyMT. */
  def getWeight: Int = parameter.weight
}
