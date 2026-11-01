package scalaprops

import ScalapropsScalaz.*
import scalaz.LazyOption
import scalaz.std.anyVal.*

object LazyOptionTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[LazyOption],
    scalazlaws.traverse.all[LazyOption],
    scalazlaws.bindRec.all[LazyOption],
    scalazlaws.zip.all[LazyOption],
    scalazlaws.isEmpty.all[LazyOption],
    scalazlaws.cobind.all[LazyOption],
    scalazlaws.align.all[LazyOption]
  )
}
