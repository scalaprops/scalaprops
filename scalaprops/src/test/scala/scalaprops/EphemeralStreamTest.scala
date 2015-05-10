package scalaprops

import scalaz.EphemeralStream
import scalaz.std.anyVal._
import scalaz.std.string._

object EphemeralStreamTest extends Scalaprops {

  val laws = Properties.either(
    "EphemeralStream",
    scalazlaws.monadPlusStrong.all[EphemeralStream],
    scalazlaws.traverse.all[EphemeralStream],
    scalazlaws.cobind.all[EphemeralStream],
    scalazlaws.align.all[EphemeralStream],
    scalazlaws.zip.all[EphemeralStream]
  )

}
