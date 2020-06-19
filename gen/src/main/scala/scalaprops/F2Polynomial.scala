package scalaprops

import java.math.BigInteger
import scala.annotation.tailrec

/**
 * Polynomial over the field of two elements. <b>F</b><sub>2</sub>[t]
 *
 * This class is immutable.
 *
 * Caution: This class is not efficient for large polynomial.
 *
 * @author M. Saito
 */
object F2Polynomial {

  /** Polynomial X<sup>1</sup> + 0. */
  private[scalaprops] val X = new F2Polynomial("10")

  /**
   * If zero, this method returns -1, otherwise, returns the degree of
   * polynomial.
   *
   * @param pol
   * polynomial
   * @return degree of polynomial pol
   */
  private def degree(pol: BigInteger): Int = {
    if (pol == BigInteger.ZERO) {
      -1
    } else {
      pol.bitLength - 1
    }
  }

  /**
   * Multiplication of BigIntegers which represents coefficient of polynomials.
   *
   * @param x
   * polynomial
   * @param y
   * polynomial
   * @return the result of multiplication
   */
  private def mul(x: BigInteger, y: BigInteger): BigInteger = {
    @tailrec def loop(z: BigInteger, v: BigInteger, w: BigInteger): BigInteger = {
      if (w != BigInteger.ZERO) {
        val z0 = if (w.and(BigInteger.ONE).testBit(0)) {
          z.xor(v)
        } else z
        loop(z0, v.shiftLeft(1), w.shiftRight(1))
      } else z
    }
    loop(BigInteger.ZERO, x, y)
  }

  /**
   * Calculate residue of polynomial y divided by that polynomial. Using means
   * return y % that if y and that were int.
   *
   * @param y dividee
   * @param that divider
   * @return residue
   */
  private def mod(y: BigInteger, that: BigInteger): BigInteger = {
    val deg = degree(that)
    val diff = degree(y) - deg
    if (diff < 0) {
      y
    } else if (diff == 0) {
      y.xor(that)
    } else {
      var z: BigInteger = y
      var x: BigInteger = that.shiftLeft(diff)
      z = z.xor(x)
      var zdeg: Int = z.bitLength - 1
      while (zdeg >= deg) {
        x = x.shiftRight(x.bitLength - 1 - zdeg)
        z = z.xor(x)
        zdeg = z.bitLength - 1
      }
      z
    }
  }

  /**
   * returns x<sup>pow</sup> % mod.
   *
   * @param x polynomial
   * @param power exponent
   * @param mod polynomial
   * @return polynomial whose degree is less than mod polynomial
   */
  private def powerMod(x: BigInteger, power: BigInteger, mod: BigInteger): BigInteger = {
    var z: BigInteger = BigInteger.ONE
    var s: BigInteger = x
    var pow: BigInteger = power
    while (!(pow == BigInteger.ZERO)) {
      if (pow.and(BigInteger.ONE).testBit(0)) {
        z = mul(z, s)
        z = this.mod(z, mod)
      }
      s = mul(s, s)
      s = this.mod(s, mod)
      pow = pow.shiftRight(1)
    }
    z
  }
}

/** @param pol internal representation of polynomial. */
final class F2Polynomial private (private val pol: BigInteger) {

  /**
   * constructor from string with radix.
   *
   * @param value
   * a string consists of numbers of radix
   * @param radix
   * radix of the number of val
   */
  def this(value: String, radix: Int) = {
    this(new BigInteger(value, radix))
  }

  /**
   * constructor from string of 0 and 1.
   *
   * @param value
   * a string consists of 0 and 1
   */
  def this(value: String) = {
    this(new BigInteger(value, 2))
  }

  /**
   * If zero, this method returns -1, otherwise, returns the degree of polynomial.
   *
   * @return degree of this polynomial
   */
  def degree: Int =
    F2Polynomial.degree(pol)

  /**
   * Addition over <b>F<sub>2</sub></b>[t].
   *
   * @param that Polynomial
   * @return result of addition
   */
  def add(that: F2Polynomial): F2Polynomial =
    new F2Polynomial(this.pol.xor(that.pol))

  /**
   * Multiplication over <b>F<sub>2</sub></b>[t].
   *
   * @param that Polynomial
   * @return result of multiplication
   */
  def mul(that: F2Polynomial): F2Polynomial =
    if (this.degree >= that.degree) {
      new F2Polynomial(F2Polynomial.mul(this.pol, that.pol))
    } else {
      new F2Polynomial(F2Polynomial.mul(that.pol, this.pol))
    }

  /**
   * return coefficient of specified term, returned value is zero or one.
   *
   * @param index degree of term
   * @return coefficient of specified term
   */
  def getCoefficient(index: Int): Int =
    if (pol.testBit(index)) {
      1
    } else {
      0
    }

  /**
   * Calculate residue of this polynomial divided by that polynomial. Using
   * means return this % that if this and that were int.
   *
   * @param that divider
   * @return residue
   */
  def mod(that: F2Polynomial): F2Polynomial =
    new F2Polynomial(F2Polynomial.mod(this.pol, that.pol))

  /**
   * power of this polynomial, this<sup>pow</sup>.
   *
   * @param pow exponent
   * @return this<sup>pow</sup>
   */
  def power(pow: BigInteger): F2Polynomial =
    new F2Polynomial(power(this.pol, pow))

  /**
   * power of polynomial x, x<sup>pow</sup>.
   *
   * @param x polynomial
   * @param power exponent
   * @return the result of power
   */
  private def power(x: BigInteger, power: BigInteger): BigInteger = {
    var z: BigInteger = BigInteger.ONE
    var v: BigInteger = x
    var pow: BigInteger = power
    while (!(pow == BigInteger.ZERO)) {
      if (pow.and(BigInteger.ONE).testBit(0)) {
        z = F2Polynomial.mul(z, v)
      }
      v = F2Polynomial.mul(v, v)
      pow = pow.shiftRight(1)
    }
    z
  }

  /**
   * returns this<sup>pow</sup> % mod.
   *
   * @param pow exponent
   * @param mod polynomial
   * @return polynomial whose degree is less than mod polynomial
   */
  def powerMod(pow: BigInteger, mod: F2Polynomial): F2Polynomial = {
    new F2Polynomial(F2Polynomial.powerMod(this.pol, pow, mod.pol))
  }

  /**
   * return binary format representation of polynomial.
   *
   * @return binary format string
   */
  override def toString: String =
    toString(2)

  /**
   * return base format representation of polynomial.
   *
   * @param base base of format
   * @return base format string
   */
  def toString(base: Int): String =
    pol.toString(base)

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: F2Polynomial =>
        this.pol == that.pol
      case _ =>
        false
    }
  }

  override def hashCode: Int =
    pol.hashCode
}
