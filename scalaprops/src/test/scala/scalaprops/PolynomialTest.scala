package scalaprops

import java.math.BigInteger

object PolynomialTest extends Scalaprops {
  property("polynomialString") = Property.forAll {
    new F2Polynomial("1101").toString == "1101"
  }

  property("polynomialStringInt") = Property.forAll {
    new F2Polynomial("f", 16).toString == "1111"
  }

  property("degree") = Property.forAll {
    assert(new F2Polynomial("100").degree == 2)
    assert(new F2Polynomial("1").degree == 0)
    assert(new F2Polynomial("0").degree == -1)
    true
  }

  property("add") = Property.forAll {
    new F2Polynomial("1101").add(new F2Polynomial("110")) == new F2Polynomial("1011")
  }

  property("mul") = Property.forAll {
    assert(new F2Polynomial("110") == new F2Polynomial("11").mul(new F2Polynomial("10")))
    assert(new F2Polynomial("1110") == new F2Polynomial("10").mul(new F2Polynomial("111")))
    true
  }

  property("mod") = Property.forAll {
    new F2Polynomial("1011").mod(new F2Polynomial("11")) == new F2Polynomial("1")
  }

  property("power") = Property.forAll {
    new F2Polynomial("10").power(new BigInteger("3")) == new F2Polynomial("1000")
  }

  property("powerMod") = Property.forAll {
    new F2Polynomial("10").powerMod(new BigInteger("3"), new F2Polynomial("11")) == new F2Polynomial("1")
  }

  property("getCoefficient") = Property.forAll {
    assert(1 == new F2Polynomial("10").getCoefficient(1))
    assert(0 == new F2Polynomial("10").getCoefficient(0))
    true
  }

  property("toString") = Property.forAll {
    assert("101" == new F2Polynomial("101").toString)
    assert("5" == new F2Polynomial("101").toString(16))
    true
  }

  property("hashCode") = Property.forAll {
    new BigInteger("1101", 2).hashCode() == new F2Polynomial("1101").hashCode()
  }

  property("equals") = Property.forAll {
    assert(new F2Polynomial("11010") == new F2Polynomial("11010"))
    assert(new F2Polynomial("11010") != new F2Polynomial("11011"))
    val p = new F2Polynomial("101")
    assert(p == p)
    true
  }
}
