package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object IMapTest extends Scalaprops {

  val testLaws =
    Properties.either(
      "==>> Laws",
      scalazlaws.bind.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.align.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.zip.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.traverse.all[({type l[a] = Int ==>> a})#l]
    )

}
