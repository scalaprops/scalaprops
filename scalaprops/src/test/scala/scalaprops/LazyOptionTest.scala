package scalaprops

import scalaz.std.anyVal._
import scalaz.LazyOption

object LazyOptionTest extends Scalaprops {

  val laws = Properties.list(
    scalazlaws.monadPlusStrong.all[LazyOption],
    scalazlaws.traverse.all[LazyOption],
    scalazlaws.zip.all[LazyOption],
    scalazlaws.cobind.all[LazyOption],
    scalazlaws.align.all[LazyOption]
  )

}
