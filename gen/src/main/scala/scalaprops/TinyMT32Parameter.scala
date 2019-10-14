package scalaprops

/**
 * This class is used to keep parameters for TinyMT32, and to get parameters
 * from resource file.
 *
 * @author M. Saito
 *
 */
object TinyMT32Parameter {

  /** hexadecimal format. */
  private final val HEX_FORMAT = 16

  /** int to float mask. */
  private final val INT_TO_FLOAT_MASK = 0x3f800000

  /** int to float shift. */
  private final val INT_TO_FLOAT_SHIFT = 9

  /** decimal format. */
  private final val DEC_FORMAT = 10

  /** default parameter. */
  private[this] val DEFAULT_PARAMETER =
    new TinyMT32Parameter("d8524022ed8dff4a8dcc50c798faba43", 0, 0x8f7011ee, 0xfc78ff1f, 0x3793fdff, 63, 0)

  /**
   * parameters for ThreadLocalRandom.
   */
  private val THREAD_LOCAL_PARAMETER: Array[TinyMT32Parameter] = Array(
    new TinyMT32Parameter("80227acb382d7b47f3714bd1223bedaf", 1, 0xda251b45, 0xfed0ffb5, 0x9b5cf7ff, 67, 0),
    new TinyMT32Parameter("db46f27d546507bdf3445acd188fa8a3", 1, 0xa55a14aa, 0xfd28ff4b, 0xc2b9efff, 67, 0),
    new TinyMT32Parameter("e1c47f40863c844be54fc078750562ef", 1, 0xa45b148a, 0xfd20ff49, 0x79aff7ff, 61, 0),
    new TinyMT32Parameter("d1346cadec1fbc329d1fe2283a577b77", 1, 0x837c106e, 0xfc18ff07, 0x5fffa9bf, 71, 0),
    new TinyMT32Parameter("c7487f9b2e8f8aaa231ac4b22b14db9b", 1, 0x817e102e, 0xfc08ff03, 0x69f3f77f, 65, 0),
    new TinyMT32Parameter("b0d7d986ce26326dbb6b0fccda28bdbb", 1, 0x68970d13, 0xfb40fed1, 0xd868edff, 71, 0),
    new TinyMT32Parameter("c0edefb49baf0424fd235ce48e8d26fb", 1, 0x45ba08b6, 0xfa28fe8b, 0x9c503dff, 69, 0),
    new TinyMT32Parameter("b24aa41c9eba05c4ffa8fc0e90438c37", 1, 0x2cd3059b, 0xf960fe59, 0xe67d73ff, 61, 0),
    new TinyMT32Parameter("a6aca2283f3e12ed69818bd95c35d00b", 1, 0x19e6033d, 0xf8c8fe33, 0x207e65ff, 61, 0),
    new TinyMT32Parameter("a86f5f29166785b075c431b027044287", 1, 0xfa041f41, 0xf7d8fdf7, 0x9c3efdff, 57, 0),
    new TinyMT32Parameter("e570ac318b37d9caa71ba4b99bad6a3b", 1, 0xf40a1e80, 0xf7a8fdeb, 0xe1cffbfd, 69, 0),
    new TinyMT32Parameter("d227f7cbfb9408765dd7da7d51790a13", 1, 0xf10f1e20, 0xf780fde1, 0xcac37fff, 71, 0),
    new TinyMT32Parameter("833689351c2ed91b5f56d10bde4b5197", 1, 0xdb251b65, 0xf6d0fdb5, 0x17bd7bff, 65, 0),
    new TinyMT32Parameter("dd68340e3a7a7b8d993c6f412b125ca7", 1, 0xc23c1846, 0xf618fd87, 0x899e7dff, 65, 0),
    new TinyMT32Parameter("9898245c4fccabd1617bb16fff089643", 1, 0xae5015cb, 0xf578fd5f, 0x93fc9ffd, 65, 1),
    new TinyMT32Parameter("e04a99bcc12b07a661440bef54402207", 1, 0x926c124c, 0xf498fd27, 0x01bbff5f, 53, 0)
  )

  /** returns default parameter of TinyMT32.
   *
   * @return default parameter
   */
  def getDefaultParameter: TinyMT32Parameter = {
    DEFAULT_PARAMETER
  }

  /**
   * returns default parameter for ThreadLocalRandom. This parameter is
   * calculated by TinyMT32DC with ID=1.
   *
   * @param threadId thread ID
   * @return TinyMT32Parameter of ID=1
   */
  def getThreadLocalParameter(threadId: Long): TinyMT32Parameter = {
    val length = THREAD_LOCAL_PARAMETER.length
    var no: Int = (threadId % length).toInt
    if (no < 0) {
      no = -no
    }
    THREAD_LOCAL_PARAMETER(no)
  }
}

/**
 * @param characteristic characteristic polynomial.
 * @param id ID of TinyMT32.
 * @param mat1 parameter mat1 of TinyMT32.
 * @param mat2 parameter mat2 of TinyMT32.
 * @param tmat parameter tmat of TinyMT32.
 * @param weight Hamming weight of characteristic polynomial.
 * @param delta Delta of TinyMT.
 */
final case class TinyMT32Parameter(
  characteristic: F2Polynomial,
  id: Int,
  mat1: Int,
  mat2: Int,
  tmat: Int,
  weight: Int,
  delta: Int
) {

  /** private constructor.
   *
   * @param pcharacteristic
   * hexadecimal format of characteristic polynomial
   * @param pid
   * parameter ID
   * @param pmat1
   * parameter mat1
   * @param pmat2
   * parameter mat2
   * @param ptmat
   * parameter tmat
   * @param pweight
   * parameter weight
   * @param pdelta
   * parameter delta
   */
  private def this(pcharacteristic: String, pid: Int, pmat1: Int, pmat2: Int, ptmat: Int, pweight: Int, pdelta: Int) {
    this(
      characteristic = new F2Polynomial(pcharacteristic, TinyMT32Parameter.HEX_FORMAT),
      id = pid,
      mat1 = pmat1,
      mat2 = pmat2,
      tmat = ptmat,
      weight = pweight,
      delta = pdelta
    )
  }

  /** return mat1 when x is odd number.
   *
   * @param x number
   * @return mat1 when x is odd else 0
   */
  def getMat1(x: Int): Int = {
    if ((x & 1) == 0) {
      0
    } else {
      mat1
    }
  }

  /** return mat2 when x is odd number.
   *
   * @param x integer
   * @return mat1 if x is odd else 0
   */
  def getMat2(x: Int): Int = {
    if ((x & 1) == 0) {
      0
    } else {
      mat2
    }
  }

  /** return tmat if x is odd number.
   *
   * @param x integer
   * @return return tmat if x is odd else 0
   */
  def getTmat(x: Int): Int = {
    if ((x & 1) == 0) {
      0
    } else {
      tmat
    }
  }

  /** return bit pattern depends on x is odd or not.
   *
   * @param x
   * integer
   * @return bit pattern depends on x is odd or not.
   */
  def getTmatFloat(x: Int): Int = {
    if ((x & 1) == 0) {
      TinyMT32Parameter.INT_TO_FLOAT_MASK
    } else {
      TinyMT32Parameter.INT_TO_FLOAT_MASK | (tmat >>> TinyMT32Parameter.INT_TO_FLOAT_SHIFT)
    }
  }

}
