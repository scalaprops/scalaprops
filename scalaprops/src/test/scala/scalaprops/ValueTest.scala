package scalaprops

import scalaz.Value
import scalaz.std.anyVal._

object ValueTest extends Scalaprops {

  val test = Properties.list(
    scalazlaws.traverse1.all[Value],
    scalazlaws.monad.all[Value],
    scalazlaws.zip.all[Value],
    scalazlaws.align.all[Value],
    scalazlaws.comonad.all[Value]
  )

}
