package scalaprops
package std

import _root_.java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, ExecutionContext}
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.scalaFuture._

object FutureTest extends Scalaprops {
  private[this] implicit def genFuture[A](implicit A: Gen[A]): Gen[Future[A]] =
    Gen.oneOf(
      A.map(Future.successful),
      genThrowable.map(Future.failed(_))
    )

  private[this] final case class SomeFailure(n: Byte) extends Exception {
    override def toString = s"SomeFailure($n)"
  }

  private[this] implicit val singleThreadExecutionContext = new ExecutionContext {
    def execute(runnable: Runnable): Unit = runnable.run
    def reportFailure(cause: Throwable): Unit = cause.printStackTrace
  }

  private[this] implicit val genThrowable: Gen[Throwable] =
    Gen[Byte].map(SomeFailure)

  private[this] implicit val cogenThrowable: Cogen[Throwable] =
    Cogen[Byte].contramap{
      case SomeFailure(n) => n
    }

  private[this] implicit def futureEqual[A](implicit A: Equal[A]): Equal[Future[A]] =
    Equal.equal{ (f1, f2) =>
      def f(future: Future[A]) =
        Await.result(future.map(\/.right).recover { case e => -\/(e) }, Duration(5, TimeUnit.SECONDS))

      (f(f1), f(f2)) match {
        case (\/-(a1), \/-(a2)) => A.equal(a1, a2)
        case (-\/(e1), -\/(e2)) => e1 == e2
        case _ => false
      }
    }

  val laws = Properties.list(
    scalazlaws.cobind.all[Future],
    scalazlaws.monadError.all[Future, Throwable],
    scalazlaws.equal.all[Future[Int]]
  )
}
