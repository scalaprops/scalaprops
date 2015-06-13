package scalaprops

import scalaz._
import scalaz.std.anyVal._

object CodensityTest extends Scalaprops {

  private[this] val e = new FunctionEqual(10)

  private[this] final class CodensityEqual[B] {
    import e._
    implicit def equal[F[_], A](implicit F: Gen[A => F[B]], E: Equal[F[B]]): Equal[Codensity[F, A]] =
      Equal[(A => F[B]) => F[B]].contramap(f => f.apply[B] _)
  }

  private[this] val E = new CodensityEqual[Int]
  import E._

  val testMaybe =
    scalazlaws.monadPlusStrong.all[({type l[a] = Codensity[Maybe, a]})#l]

  val testIList =
    scalazlaws.monadPlusStrong.all[({type l[a] = Codensity[IList, a]})#l].andThenParam(Param.maxSize(10))

  val monadTrans = scalazlaws.monadTrans.all[Codensity]
}
