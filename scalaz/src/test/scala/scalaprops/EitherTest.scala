package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.either.*

@scalajs.js.annotation.JSExportAll
object EitherTest extends Scalaprops {
  val bitraverse = scalazlaws.bitraverse.all[Either]
  val associative = scalazlaws.associative.all[Either]
  val order = scalazlaws.order.all[Int Either Int]

  val laws1 = {
    type T[A] = Either[Int, A]
    Properties.list(
      scalazlaws.monadError.all[({ type l[a] = Either[Byte, a] })#l, Byte],
      scalazlaws.bindRec.all[T],
      scalazlaws.traverse.all[T]
    )
  }
}
