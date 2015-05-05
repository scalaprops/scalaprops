package scalaprops
package std

import _root_.java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.scalaFuture._

object FutureTest extends Scalaprops {
  private[this] implicit def genFuture[A](implicit A: Gen[A]) =
    A.map(Future.successful)

  private[this] implicit def equalFuture[A: Equal]: Equal[Future[A]] =
    Equal.equalBy(Await.result(_, Duration(5, TimeUnit.SECONDS)))

  val testLaw = scalazlaws.monad.all[Future]
}
