package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.option.*

@scalajs.js.annotation.JSExportAll
object OptionTest extends Scalaprops {
  val testLaws =
    Properties.list(
      scalazlaws.monadPlusStrong.all[Option],
      scalazlaws.traverse.all[Option],
      scalazlaws.zip.all[Option],
      scalazlaws.bindRec.all[Option],
      scalazlaws.align.all[Option],
      scalazlaws.isEmpty.all[Option],
      scalazlaws.cobind.all[Option]
    )
}
