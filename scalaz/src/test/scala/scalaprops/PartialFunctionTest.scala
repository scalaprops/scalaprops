package scalaprops

import scalaz._
import scalaz.std.partialFunction._
import scalaz.std.option._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import FunctionEqual._

@scalajs.js.annotation.JSExportAll
object PartialFunctionTest extends Scalaprops {
  private[this] implicit def equal[A: Gen, B: Equal]: Equal[PartialFunction[A, B]] =
    Equal[A => Option[B]].contramap(_.lift)

  val law = scalazlaws.arrow.all[PartialFunction]
}
