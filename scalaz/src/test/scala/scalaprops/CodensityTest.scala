package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object CodensityTest extends Scalaprops {
  private[this] final class CodensityEqual[B] {
    import FunctionEqual.*
    implicit def equal[F[_], A](implicit F: Gen[A => F[B]], E: Equal[F[B]]): Equal[Codensity[F, A]] =
      Equal[(A => F[B]) => F[B]].contramap(f => f.apply[B] _)
  }

  private[this] val E = new CodensityEqual[Int]
  import E.*

  val testMaybe =
    scalazlaws.monadPlusStrong.all[({ type l[a] = Codensity[Maybe, a] })#l]

  val testIList =
    scalazlaws.monadPlusStrong.all[({ type l[a] = Codensity[IList, a] })#l].andThenParam(Param.maxSize(10))

  val monadTrans = scalazlaws.monadTrans.all[Codensity].andThenParamPF { case ScalazLaw.monadTransLaw2IList =>
    Param.maxSize(5)
  }
}
