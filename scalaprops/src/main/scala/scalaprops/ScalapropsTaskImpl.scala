package scalaprops

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import sbt.testing._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import scalaprops.internal._

final class ScalapropsTaskImpl(
  override val taskDef: TaskDef,
  testClassLoader: ClassLoader,
  args: Array[String],
  arguments: Arguments,
  results: ArrayBuffer[TestResult],
  status: TestStatus
) extends sbt.testing.Task {
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

    TestExecutorImpl.withExecutor(log) { executor =>
      val result = ScalapropsTaskImpl.createTree(
        tests = tests,
        testClassName = testClassName,
        arguments = arguments,
        results = results,
        testStatus = status,
        eventHandler = eventHandler,
        log = log,
        obj = obj,
        fingerprint = taskDef.fingerprint(),
        executor = executor
      )
      obj.listener.onFinishAll(obj, result, log)
      Array()
    }
  }

  override def tags() = Array()
}

object ScalapropsTaskImpl {
  private[this] val emptyThrowable = new OptionalThrowable

  def createTree(
    tests: Properties[_],
    testClassName: String,
    arguments: Arguments,
    results: ArrayBuffer[TestResult],
    testStatus: TestStatus,
    eventHandler: EventHandler,
    log: Logger,
    obj: Scalaprops,
    fingerprint: Fingerprint,
    executor: TestExecutor
  ): Tree[(Any, LazyOpt[(Property, Param, ScalapropsEvent)])] = {
    tests.props.zipper.cojoin.toTree.map { t =>
      (t.tree.rootLabel +: t.parents.map(_._2)).map(_._1).reverse.mkString(".") -> t.tree.rootLabel
    }.map {
      case (fullName, (id, checkOpt)) =>
        val name = id.toString
        (id: Any) -> (checkOpt match {
          case Some(check) =>
            LazyOpt.lazySome {
              val cancel = new AtomicBoolean(false)
              val selector = new TestSelector(name)

              def event(status: Status, duration: Long, result0: CheckResultError) = {
                status match {
                  case Status.Success =>
                    testStatus.success.incrementAndGet()
                  case Status.Error =>
                    testStatus.error.incrementAndGet()
                  case Status.Failure =>
                    testStatus.failure.incrementAndGet()
                  case Status.Ignored =>
                    testStatus.ignored.incrementAndGet()
                  case Status.Pending | Status.Skipped | Status.Canceled =>
                }
                val err = result0.error match {
                  case Some(e) => new OptionalThrowable(e)
                  case None => emptyThrowable
                }
                ScalapropsEvent(fullName.toString, fingerprint, selector, status, err, duration, result0)
              }

              val param = arguments.param.merge(check.paramEndo(obj.param))
              val start = System.currentTimeMillis()
              val r =
                try {
                  obj.listener.onStart(obj, name, check.prop, param, log)
                  val r = executor.execute(param.timeout) {
                    check.prop.check(
                      param, { () => cancel.get || ((System.currentTimeMillis() - start) > param.timeout.toMillis) },
                      count => obj.listener.onCheck(obj, name, check.prop, param, log, count)
                    )
                  }
                  val duration = System.currentTimeMillis() - start
                  obj.listener.onFinish(obj, name, check.prop, param, r, log)
                  r match {
                    case _: CheckResult.Proven | _: CheckResult.Passed =>
                      event(Status.Success, duration, CheckResultError.Value(r))
                    case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
                      event(Status.Failure, duration, CheckResultError.Value(r))
                    case e: CheckResult.GenException =>
                      log.trace(e.exception)
                      event(Status.Error, duration, CheckResultError.Both(e.exception, r))
                    case e: CheckResult.PropException =>
                      log.trace(e.exception)
                      event(Status.Error, duration, CheckResultError.Both(e.exception, r))
                    case _: CheckResult.Timeout =>
                      event(Status.Error, duration, CheckResultError.Value(r))
                    case _: CheckResult.Ignored =>
                      event(Status.Ignored, duration, CheckResultError.Value(r))
                  }
                } catch {
                  case e: TimeoutException =>
                    val duration = System.currentTimeMillis() - start
                    log.trace(e)
                    obj.listener.onError(obj, name, e, log)
                    event(Status.Error, duration, CheckResultError.Err(e))
                  case NonFatal(e) =>
                    val duration = System.currentTimeMillis() - start
                    log.trace(e)
                    obj.listener.onError(obj, name, e, log)
                    event(Status.Error, duration, CheckResultError.Err(e))
                } finally {
                  cancel.set(true)
                  testStatus.all.incrementAndGet()
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
          case None =>
            LazyOpt.lazyNone
        })
    }
  }

  private[scalaprops] def filterTests(
    objName: String,
    tests: List[Properties[Any]],
    names: List[String],
    logger: Logger
  ): List[Properties[Any]] = {
    val set = names.toSet
    val actualTests = tests.map(_.id.toString).toSet
    set.filterNot(actualTests).foreach { typo => logger.warn(s"""'${objName}.$typo' does not exists""") }
    tests.filter(p => set(p.id.toString))
  }
}
