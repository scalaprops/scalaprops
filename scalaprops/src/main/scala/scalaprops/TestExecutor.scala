package scalaprops

import scala.concurrent.duration.Duration

abstract class TestExecutor {
  def execute[A](timeout: Duration)(f: => A): A
  def shutdown(): Unit
}
