package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.either.*

object EitherTest extends Scalaprops {
  property("bitraverse") = scalazlaws.bitraverse.all[Either]
  property("associative") = scalazlaws.associative.all[Either]
  property("order") = scalazlaws.order.all[Int Either Int]

  property("laws1") = {
    type T[A] = Either[Int, A]
    Properties.list(
      scalazlaws.monadError.all[({ type l[a] = Either[Byte, a] })#l, Byte],
      scalazlaws.bindRec.all[T],
      scalazlaws.traverse.all[T]
    )
  }
}
