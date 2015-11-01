package scalaprops

import scalaz.Name
import scalaz.std.anyVal._

object NameTest extends Scalaprops {

  val test = Properties.list(
    scalazlaws.traverse1.all[Name],
    scalazlaws.monad.all[Name],
    scalazlaws.bindRec.all[Name],
    scalazlaws.zip.all[Name],
    scalazlaws.align.all[Name],
    scalazlaws.comonad.all[Name]
  )

}
