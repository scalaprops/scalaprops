package scalaprops

import sbt.testing.Logger
import scala.concurrent.duration.Duration

object TestExecutorImpl {
  val instance = new TestExecutor {
    override def execute[A](timeout: Duration)(f: => A): A = f
    override def shutdown(): Unit = {}
  }
  def withExecutor[A](logger: Logger)(f: TestExecutor => A): A =
    f(instance)
}
