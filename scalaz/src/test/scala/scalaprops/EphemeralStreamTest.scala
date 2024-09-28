package scalaprops

import scalaz.EphemeralStream
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object EphemeralStreamTest extends Scalaprops {
  val bindRec = scalazlaws.bindRec.laws[EphemeralStream].andThenParam(Param.maxSize(1))

  val laws = Properties.list(
    scalazlaws.monadPlusStrong.all[EphemeralStream],
    scalazlaws.traverse.all[EphemeralStream],
    scalazlaws.cobind.all[EphemeralStream],
    scalazlaws.isEmpty.all[EphemeralStream],
    scalazlaws.align.all[EphemeralStream],
    scalazlaws.zip.all[EphemeralStream]
  )
}
