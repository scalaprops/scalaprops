package scalaprops

import FunctionEqual.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object RepresentableTest extends Scalaprops {
  val reader = scalazlaws.representable.all[({ type l[a] = Byte => a })#l, Byte]
  val curry = scalazlaws.representable.all[({ type l[a] = Byte => a })#l, (Byte, Unit)]
}
