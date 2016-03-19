package scalaprops

import scala.concurrent.duration.Duration
import sbt.testing.Logger

object TestExecutorImpl {
  private[this] val instance = new TestExecutor {
    override def execute[A](timeout: Duration)(f: => A): A = f
    override def shutdown(): Unit = {}
  }
  def withExecutor[A](logger: Logger)(f: TestExecutor => A): A =
    f(instance)
}
