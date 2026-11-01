package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*

object NeedTest extends Scalaprops {
  property("test") = Properties.list(
    scalazlaws.traverse1.all[scalaz.Need],
    scalazlaws.monad.all[scalaz.Need],
    scalazlaws.bindRec.all[scalaz.Need],
    scalazlaws.zip.all[scalaz.Need],
    scalazlaws.align.all[scalaz.Need],
    scalazlaws.comonad.all[scalaz.Need]
  )
}
