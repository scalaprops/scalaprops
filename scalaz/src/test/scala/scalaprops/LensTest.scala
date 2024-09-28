package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object LensTest extends Scalaprops {
  import FunctionEqual._
  import IndexedStoreTTest.indexedStoreTEqual

  private[this] implicit def lensEqual[A, B, C, D](implicit
    A: Equal[A => IndexedStore[C, D, B]]
  ): Equal[LensFamily[A, B, C, D]] =
    A.contramap[LensFamily[A, B, C, D]](_.run)

  val testCategoryLaw = {
    implicit val l = Gen.value(Lens.lensId[Int])
    scalazlaws.category.all[Lens]
  }

  val testId = scalazlaws.lens.all(Lens.lensId[Int])
  val testTrivial = scalazlaws.lens.all(Lens.trivialLens[Int])
  val testCodiag = scalazlaws.lens.all(Lens.codiagLens[Int])
  val testFirst = scalazlaws.lens.all(Lens.firstLens[Int, Int])
  val testSecond = scalazlaws.lens.all(Lens.secondLens[Int, Int])
}
