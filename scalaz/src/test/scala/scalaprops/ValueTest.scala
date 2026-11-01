package scalaprops

import ScalapropsScalaz.*
import scalaz.Value
import scalaz.std.anyVal.*

object ValueTest extends Scalaprops {
  property("test") = Properties.list(
    scalazlaws.traverse1.all[Value],
    scalazlaws.monad.all[Value],
    scalazlaws.bindRec.all[Value],
    scalazlaws.zip.all[Value],
    scalazlaws.align.all[Value],
    scalazlaws.comonad.all[Value]
  )
}
