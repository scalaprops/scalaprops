package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

object LensTest extends Scalaprops {
  import FunctionEqual.*
  import IndexedStoreTTest.indexedStoreTEqual

  private[this] implicit def lensEqual[A, B, C, D](implicit
    A: Equal[A => IndexedStore[C, D, B]]
  ): Equal[LensFamily[A, B, C, D]] =
    A.contramap[LensFamily[A, B, C, D]](_.run)

  property("categoryLaw") = {
    implicit val l = Gen.value(Lens.lensId[Int])
    scalazlaws.category.all[Lens]
  }

  property("id") = scalazlaws.lens.all(Lens.lensId[Int])
  property("trivial") = scalazlaws.lens.all(Lens.trivialLens[Int])
  property("codiag") = scalazlaws.lens.all(Lens.codiagLens[Int])
  property("first") = scalazlaws.lens.all(Lens.firstLens[Int, Int])
  property("second") = scalazlaws.lens.all(Lens.secondLens[Int, Int])
}
