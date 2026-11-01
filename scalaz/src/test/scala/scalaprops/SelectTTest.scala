package scalaprops

import FunctionEqual.*
import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object SelectTTest extends Scalaprops {
  private[this] implicit def selectTEqual[R, M[_], A](implicit
    F: Equal[(A => M[R]) => M[A]]
  ): Equal[SelectT[R, M, A]] =
    F.contramap(_.run)

  property("maybe") = scalazlaws.monadPlus.all[({ type l[a] = SelectT[Byte, Maybe, a] })#l]
  property("iList") = scalazlaws.monadPlus.all[({ type l[a] = SelectT[Byte, IList, a] })#l]

  property("monadTrans") = scalazlaws.monadTrans.all[({ type l[f[_], a] = SelectT[Byte, f, a] })#l]
}
