package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object ListTTest extends Scalaprops {
  val testMaybe =
    scalazlaws.monad.all[({ type l[a] = ListT[Maybe, a] })#l]

  val testDisableTestList =
    scalazlaws.bind
      .laws[({ type l[a] = ListT[IList, a] })#l]
      .andThenParam(Param.maxSize(2))
      .ignore("https://github.com/scalaz/scalaz/issues/921")

  val monadTrans = scalazlaws.monadTrans.all[ListT]
}
