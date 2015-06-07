package scalaprops

import scalaz._
import scalaz.std.anyVal._

object IMapTest extends Scalaprops {

  val testLaws =
    Properties.list(
      scalazlaws.bind.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.align.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.zip.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.traverse.all[({type l[a] = Int ==>> a})#l]
    )

}
