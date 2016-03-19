package scalaprops

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import sbt.testing._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import scalaz._

final class ScalapropsTaskImpl(
  override val taskDef: TaskDef,
  testClassLoader: ClassLoader,
  args: Array[String],
  arguments: Arguments,
  results: ArrayBuffer[TestResult],
  status: TestStatus
) extends sbt.testing.Task {

  private[this] val emptyThrowable = new OptionalThrowable

  def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: Array[Task] => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  override def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {
    val log = Scalaprops.logger(loggers)

    val testClassName = taskDef.fullyQualifiedName()

    val obj = ScalapropsRunner.getTestObject(
      fingerprint = taskDef.fingerprint,
      testClassName = testClassName,
      testClassLoader = testClassLoader
    )
    val tests = ScalapropsRunner.findTests(
      fingerprint = taskDef.fingerprint,
      testClassName = testClassName,
      testClassLoader = testClassLoader,
      only = arguments.only,
      logger = log
    )

    TestExecutorImpl.withExecutor(log){ executor =>
      val result = tests.props.loc.cojoin.toTree.map{ t =>
        (t.tree.rootLabel +: t.parents.map(_._2)).map(_._1).reverse.mkString(".") -> t.tree.rootLabel
      }.map{ case (fullName, (id, checkOpt)) =>
        val name = id.toString
        (id: Any) -> (checkOpt match {
          case Maybe.Just(check) => LazyOption.lazySome{
            val cancel = new AtomicBoolean(false)
            val selector = new TestSelector(name)
            def event(status: Status, duration: Long, result0: Throwable \&/ CheckResult) = {
              status match {
                case Status.Success =>
                  this.status.success.incrementAndGet()
                case Status.Error =>
                  this.status.error.incrementAndGet()
                case Status.Failure =>
                  this.status.failure.incrementAndGet()
                case Status.Ignored =>
                  this.status.ignored.incrementAndGet()
                case Status.Pending | Status.Skipped | Status.Canceled =>
              }
              val err = result0.a match {
                case Some(e) => new OptionalThrowable(e)
                case None => emptyThrowable
              }
              ScalapropsEvent(testClassName, taskDef.fingerprint(), selector, status, err, duration, result0)
            }

            val param = check.paramEndo(obj.param)
            val start = System.currentTimeMillis()
            val r = try {
              obj.listener.onStart(obj, name, check.prop, param, log)
              val r = executor.execute(param.timeout){
                check.prop.check(
                  param,
                  { () =>
                    cancel.get || ((System.currentTimeMillis() - start) > param.timeout.toMillis)
                  },
                  count => obj.listener.onCheck(obj, name, check, log, count)
                )
              }
              val duration = System.currentTimeMillis() - start
              obj.listener.onFinish(obj, name, check.prop, param, r, log)
              r match {
                case _: CheckResult.Proven | _: CheckResult.Passed =>
                  event(Status.Success, duration, \&/.That(r))
                case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
                  event(Status.Failure, duration, \&/.That(r))
                case e: CheckResult.GenException =>
                  log.trace(e.exception)
                  event(Status.Error, duration, \&/.Both(e.exception, r))
                case e: CheckResult.PropException =>
                  log.trace(e.exception)
                  event(Status.Error, duration, \&/.Both(e.exception, r))
                case e: CheckResult.Timeout =>
                  event(Status.Error, duration, \&/.That(r))
                case e: CheckResult.Ignored =>
                  event(Status.Ignored, duration, \&/.That(r))
              }
            } catch {
              case e: TimeoutException =>
                val duration = System.currentTimeMillis() - start
                log.trace(e)
                obj.listener.onError(obj, name, e, log)
                event(Status.Error, duration, \&/.This(e))
              case NonFatal(e) =>
                val duration = System.currentTimeMillis() - start
                log.trace(e)
                obj.listener.onError(obj, name, e, log)
                event(Status.Error, duration, \&/.This(e))
            } finally {
              cancel.set(true)
              status.all.incrementAndGet()
            }
            eventHandler.handle(r)
            results += TestResult(
              name = fullName,
              duration = r.duration,
              maxSize = param.maxSize,
              minSuccessful = param.minSuccessful
            )
            (check.prop, param, r)
          }
          case Maybe.Empty() =>
            LazyOption.lazyNone
        })
      }
      obj.listener.onFinishAll(obj, result, log)
      Array()
    }
  }

  override def tags() = Array()
}
