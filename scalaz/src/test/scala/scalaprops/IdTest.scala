package scalaprops

import ScalapropsScalaz.*
import scalaz.Id.Id
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object IdTest extends Scalaprops {
  val test = Properties.list(
    scalazlaws.traverse1.all[Id],
    scalazlaws.monad.all[Id],
    scalazlaws.bindRec.all[Id],
    scalazlaws.zip.all[Id],
    scalazlaws.align.all[Id],
    scalazlaws.comonad.all[Id]
  )
}
