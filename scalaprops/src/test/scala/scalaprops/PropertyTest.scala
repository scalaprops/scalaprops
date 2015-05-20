package scalaprops

import java.util.concurrent.atomic.AtomicBoolean

object PropertyTest extends Scalaprops{

  val `"forAll(result: => Boolean)" is lazy` = Property.forAll{
    var sideEffect = false
    val p = Property.forAll{
      sideEffect = true
      true
    }
    assert(sideEffect == false)
    p.check(Param.withCurrentTimeSeed(), new AtomicBoolean(), _ => ())
    sideEffect
  }

}
