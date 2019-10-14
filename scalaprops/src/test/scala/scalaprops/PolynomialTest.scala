package scalaprops

import java.math.BigInteger

object PolynomialTest extends Scalaprops {

  val testPolynomialString = Property.forAll {
    new F2Polynomial("1101").toString() == "1101"
  }

  val testPolynomialStringInt = Property.forAll {
    new F2Polynomial("f", 16).toString == "1111"
  }

  val testDegree = Property.forAll {
    assert(new F2Polynomial("100").degree == 2)
    assert(new F2Polynomial("1").degree == 0)
    assert(new F2Polynomial("0").degree == -1)
    true
  }

  val testAdd = Property.forAll {
    new F2Polynomial("1101").add(new F2Polynomial("110")) == new F2Polynomial("1011")
  }

  val testMul = Property.forAll {
    assert(new F2Polynomial("110") == new F2Polynomial("11").mul(new F2Polynomial("10")))
    assert(new F2Polynomial("1110") == new F2Polynomial("10").mul(new F2Polynomial("111")))
    true
  }

  val testMod = Property.forAll {
    new F2Polynomial("1011").mod(new F2Polynomial("11")) == new F2Polynomial("1")
  }

  val testPower = Property.forAll {
    new F2Polynomial("10").power(new BigInteger("3")) == new F2Polynomial("1000")
  }

  val testPowerMod = Property.forAll {
    new F2Polynomial("10").powerMod(new BigInteger("3"), new F2Polynomial("11")) == new F2Polynomial("1")
  }

  val testGetCoefficient = Property.forAll {
    assert(1 == new F2Polynomial("10").getCoefficient(1))
    assert(0 == new F2Polynomial("10").getCoefficient(0))
    true
  }

  val testToString = Property.forAll {
    assert("101" == new F2Polynomial("101").toString())
    assert("5" == new F2Polynomial("101").toString(16))
    true
  }

  val testHashCode = Property.forAll {
    new BigInteger("1101", 2).hashCode() == new F2Polynomial("1101").hashCode()
  }

  val testEquals = Property.forAll {
    assert(new F2Polynomial("11010") == new F2Polynomial("11010"))
    assert(new F2Polynomial("11010") != new F2Polynomial("11011"))
    assert(new F2Polynomial("11010") != new BigInteger("11010", 2))
    val p = new F2Polynomial("101")
    assert(p == p)
    true
  }
}
