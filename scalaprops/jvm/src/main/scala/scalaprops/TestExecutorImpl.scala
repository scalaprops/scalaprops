package scalaprops

import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.ForkJoinPool
import sbt.testing.Logger
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration

object TestExecutorImpl {
  private[this] def newInstance(log: Logger): TestExecutor =
    new TestExecutor {
      private[this] val executionContext = {
        lazy val executorService: ForkJoinPool = new ForkJoinPool(
          sys.runtime.availableProcessors(),
          ForkJoinPool.defaultForkJoinWorkerThreadFactory,
          new UncaughtExceptionHandler {
            def uncaughtException(t: Thread, e: Throwable): Unit = {
              log.error("uncaughtException Thread = " + t)
              log.trace(e)
              e.printStackTrace()
              executorService.shutdown()
            }
          },
          false
        )
        ExecutionContext.fromExecutorService(executorService)
      }

      override def execute[A](timeout: Duration)(f: => A): A =
        Await.result(Future(f)(executionContext), timeout)

      override def shutdown(): Unit =
        executionContext.shutdown()
    }
  def withExecutor[A](logger: Logger)(f: TestExecutor => A): A = {
    val executor = newInstance(logger)
    try f(executor)
    finally executor.shutdown()
  }
}
