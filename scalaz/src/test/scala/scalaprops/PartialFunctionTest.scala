package scalaprops

import FunctionEqual.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.option.*
import scalaz.std.partialFunction.*
import scalaz.std.tuple.*

@scalajs.js.annotation.JSExportAll
object PartialFunctionTest extends Scalaprops {
  private[this] implicit def equal[A: Gen, B: Equal]: Equal[PartialFunction[A, B]] =
    Equal[A => Option[B]].contramap(_.lift)

  val law = scalazlaws.arrow.all[PartialFunction]
}
