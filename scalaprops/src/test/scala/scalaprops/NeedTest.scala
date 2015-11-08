package scalaprops

import scalaz.Need
import scalaz.std.anyVal._

object NeedTest extends Scalaprops {

  val test = Properties.list(
    scalazlaws.traverse1.all[Need],
    scalazlaws.monad.all[Need],
    scalazlaws.zip.all[Need],
    scalazlaws.align.all[Need],
    scalazlaws.comonad.all[Need]
  )

}
