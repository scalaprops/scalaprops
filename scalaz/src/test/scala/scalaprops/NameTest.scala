package scalaprops

import ScalapropsScalaz.*
import scalaz.Name
import scalaz.std.anyVal.*

object NameTest extends Scalaprops {
  property("test") = Properties.list(
    scalazlaws.traverse1.all[Name],
    scalazlaws.monad.all[Name],
    scalazlaws.bindRec.all[Name],
    scalazlaws.zip.all[Name],
    scalazlaws.align.all[Name],
    scalazlaws.comonad.all[Name]
  )
}
