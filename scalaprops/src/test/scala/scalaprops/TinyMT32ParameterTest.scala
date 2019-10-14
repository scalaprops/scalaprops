package scalaprops

object TinyMT32ParameterTest extends Scalaprops {

  val testGetThreadLocalParameter = Property.forAll {
    val p0 = TinyMT32Parameter.getThreadLocalParameter(0)
    val p1 = TinyMT32Parameter.getThreadLocalParameter(1)
    val pm2 = TinyMT32Parameter.getThreadLocalParameter(-2)
    assert(p0.mat1 != p1.mat1)
    p0.mat1 != pm2.mat1
  }

  val testGetDefaultParameter = Property.forAll {
    val p = TinyMT32Parameter.getDefaultParameter
    //d8524022ed8dff4a8dcc50c798faba43,32,0,8f7011ee,fc78ff1f,3793fdff,63,0
    val pol = new F2Polynomial("d8524022ed8dff4a8dcc50c798faba43", 16)
    val id = 0
    val mat1 = 0x8f7011ee
    val mat2 = 0xfc78ff1f
    val tmat = 0x3793fdff
    val weight = 63
    val delta = 0
    assert(pol == p.characteristic)
    assert(id == p.id)
    assert(mat1 == p.mat1)
    assert(mat1 == p.getMat1(1))
    assert(0 == p.getMat1(2))
    assert(mat2 == p.mat2)
    assert(mat2 == p.getMat2(3))
    assert(0 == p.getMat2(4))
    assert(tmat == p.tmat)
    assert(tmat == p.getTmat(5))
    assert(0 == p.getTmat(6))
    assert((0x3f800000 | (tmat >>> 9)) == p.getTmatFloat(7))
    assert(0x3f800000 == p.getTmatFloat(8))
    assert(weight == p.weight)
    assert(delta == p.delta)
    true
  }

}
