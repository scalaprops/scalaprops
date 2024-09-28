package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.stream._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object ShrinkTest extends Scalaprops {
  private[this] implicit def equal[A: Gen: Equal]: Equal[Shrink[A]] = {
    import FunctionEqual._
    Equal[A => Stream[A]].contramap(_.f)
  }

  val law = scalazlaws.invariantFunctor.all[Shrink]

  val int = {
    val x = 128

    Property.property { (seed: Int) =>
      val param0 = super.param.copy(
        seed = Seed.IntSeed(seed)
      )

      val m = 6
      val g = Gen.choose(List.fill(m)(2).product, Int.MaxValue)

      Property.forAllG(Gen.choose(1, x)) { (n: Int) =>
        val result =
          Property.property1[Int] { i => Property.prop(i < n) }(g, implicitly).check(param0, () => false, _ => ())

        result match {
          case CheckResult.Falsified(_, 0, arg :: Nil, _)
              if (m < arg.shrinks) && (arg.shrinks < Integer.SIZE) && (arg.value == n) =>
            true
          case fail =>
            sys.error(fail.toString)
        }
      }
    }(implicitly, Shrink.empty)
  }
}
