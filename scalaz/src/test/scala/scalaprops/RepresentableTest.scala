package scalaprops

import FunctionEqual.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object RepresentableTest extends Scalaprops {
  property("reader") = scalazlaws.representable.all[({ type l[a] = Byte => a })#l, Byte]
  property("curry") = scalazlaws.representable.all[({ type l[a] = Byte => a })#l, (Byte, Unit)]
}
