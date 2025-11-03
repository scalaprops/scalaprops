package scalaprops

import FunctionEqual.*
import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object SelectTTest extends Scalaprops {
  private[this] implicit def selectTEqual[R, M[_], A](implicit
    F: Equal[(A => M[R]) => M[A]]
  ): Equal[SelectT[R, M, A]] =
    F.contramap(_.run)

  val maybe = scalazlaws.monadPlus.all[({ type l[a] = SelectT[Byte, Maybe, a] })#l]
  val iList = scalazlaws.monadPlus.all[({ type l[a] = SelectT[Byte, IList, a] })#l]

  val monadTrans = scalazlaws.monadTrans.all[({ type l[f[_], a] = SelectT[Byte, f, a] })#l]
}
