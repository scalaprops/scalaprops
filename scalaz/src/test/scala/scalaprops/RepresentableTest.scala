package scalaprops

import FunctionEqual.*
import scalaz.std.anyVal.*

object RepresentableTest extends Scalaprops {
  property("reader") = scalazlaws.representable.all[({ type l[a] = Byte => a })#l, Byte]
  property("curry") = scalazlaws.representable.all[({ type l[a] = Byte => a })#l, (Byte, Unit)]
}
