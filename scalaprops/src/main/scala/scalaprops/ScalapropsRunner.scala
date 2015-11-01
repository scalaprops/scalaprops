package scalaprops

import java.lang.Thread.UncaughtExceptionHandler
import java.lang.reflect.Method
import java.util.concurrent.{TimeoutException, ForkJoinPool}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import sbt.testing._
import scala.util.control.NonFatal
import scalaz._
import scala.reflect.NameTransformer

object ScalapropsRunner {

  def testFieldNames(clazz: Class[_]): Array[String] =
    Array(
      findTestFields(clazz, classOf[Property]),
      findTestFields(clazz, classOf[Properties[_]])
    ).flatten.map(_.getName)

  private[this] def findTestFields(clazz: Class[_], fieldType: Class[_]): Array[Method] =
    clazz.getMethods.filter(method =>
      method.getParameterTypes.length == 0 && method.getReturnType == fieldType
    )

  private[this] def invokeProperty(clazz: Class[_], obj: Scalaprops): List[(String, Property)] =
    findTestFields(clazz, classOf[Property]).map{ method =>
      val p = method.invoke(obj).asInstanceOf[Property]
      NameTransformer.decode(method.getName) -> p
    }.toList

  private[this] def invokeProperties(clazz: Class[_], obj: Scalaprops): List[Properties[Any]] =
    findTestFields(clazz, classOf[Properties[_]]).map{ method =>
      val methodName = NameTransformer.decode(method.getName)
      val props = method.invoke(obj).asInstanceOf[Properties[Any]].props
      Properties.noSort[Any](
        Tree.Node(
          methodName -> Maybe.empty,
          props #:: Stream.empty
        )
      )
    }(collection.breakOut)

  private def allProps(clazz: Class[_], obj: Scalaprops, only: Option[NonEmptyList[String]], logger: Logger): Properties[_] = {
    val tests0 = invokeProperty(clazz, obj).map {
      case (name, p) => p.toProperties[Any](name)
    } ::: invokeProperties(clazz, obj)

    val tests = only match {
      case Some(names) =>
        val set = Foldable[NonEmptyList].toSet(names)
        val actualTests: Set[String] = tests0.map(_.id.toString)(collection.breakOut)
        set.filterNot(actualTests).foreach{ typo =>
          logger.warn(s"""'${clazz.getCanonicalName.dropRight(1)}.$typo' does not exists""")
        }
        tests0.filter(p => set(p.id.toString))
      case None =>
        tests0
    }

    Properties.noSort[Any](
      Tree.Node(
        clazz.getName -> Maybe.empty,
        obj.transformProperties(tests).map(_.props)(collection.breakOut)
      )
    )
  }

  private def logger(loggers: Array[Logger]): Logger = new Logger {
    override def warn(msg: String): Unit =
      loggers.foreach(_.warn(msg))
    override def error(msg: String): Unit =
      loggers.foreach(_.error(msg))
    override def ansiCodesSupported(): Boolean =
      loggers.forall(_.ansiCodesSupported())
    override def debug(msg: String): Unit =
      loggers.foreach(_.debug(msg))
    override def trace(t: Throwable): Unit =
      loggers.foreach(_.trace(t))
    override def info(msg: String): Unit =
      loggers.foreach(_.info(msg))
  }

}

final class ScalapropsRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader
) extends Runner {

  private[this] val successCount = new AtomicInteger
  private[this] val failureCount = new AtomicInteger
  private[this] val errorCount = new AtomicInteger
  private[this] val ignoredCount = new AtomicInteger
  private[this] val testCount = new AtomicInteger

  private[this] val taskdef2task: TaskDef => sbt.testing.Task = { taskdef =>
    val testClassName = taskdef.fullyQualifiedName()
    val emptyThrowable = new OptionalThrowable

    new sbt.testing.Task {
      override def taskDef() = taskdef

      override def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {
        val log = ScalapropsRunner.logger(loggers)

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

        val only = scalaz.std.list.toNel(
          args.dropWhile("--only" != _).drop(1).takeWhile(arg => !arg.startsWith("--")).toList
        )

        try {
          val clazz = testClassLoader.loadClass(testClassName + "$")
          val obj = clazz.getDeclaredField("MODULE$").get(null).asInstanceOf[Scalaprops]
          val test = ScalapropsRunner.allProps(clazz, obj, only, log)
          val result = test.props.map { case (id, checkOpt) =>
            val name = id.toString // TODO create type class ?
            (id: Any) -> (checkOpt match{
              case Maybe.Just(check) => LazyOption.lazySome{
                val cancel = new AtomicBoolean(false)
                val selector = new TestSelector(name)
                def event(status: Status, duration: Long, result0: Throwable \&/ CheckResult) = {
                  val err = result0.a match {
                    case Some(e) => new OptionalThrowable(e)
                    case None => emptyThrowable
                  }
                  ScalapropsEvent(testClassName, taskdef.fingerprint(), selector, status, err, duration, result0)
                }

                val param = check.paramEndo(obj.param)
                val start = System.currentTimeMillis()
                val r = try {
                  obj.listener.onStart(obj, name, check.prop, param, log)
                  val r = scalaz.concurrent.Task(
                    check.prop.check(
                      param,
                      cancel,
                      count => obj.listener.onCheck(obj, name, check, log, count)
                    )
                  )(executorService).unsafePerformSyncFor(param.timeout)
                  val duration = System.currentTimeMillis() - start
                  obj.listener.onFinish(obj, name, check.prop, param, r, log)
                  r match {
                    case _: CheckResult.Proven | _: CheckResult.Passed =>
                      successCount.incrementAndGet()
                      event(Status.Success, duration, \&/.That(r))
                    case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
                      failureCount.incrementAndGet()
                      event(Status.Failure, duration, \&/.That(r))
                    case e: CheckResult.GenException =>
                      log.trace(e.exception)
                      errorCount.incrementAndGet()
                      event(Status.Error, duration, \&/.Both(e.exception, r))
                    case e: CheckResult.PropException =>
                      log.trace(e.exception)
                      errorCount.incrementAndGet()
                      event(Status.Error, duration, \&/.Both(e.exception, r))
                    case e: CheckResult.Timeout =>
                      errorCount.incrementAndGet()
                      event(Status.Error, duration, \&/.That(r))
                    case e: CheckResult.Ignored =>
                      ignoredCount.incrementAndGet()
                      event(Status.Ignored, duration, \&/.That(r))
                  }
                } catch {
                  case e: TimeoutException =>
                    val duration = System.currentTimeMillis() - start
                    log.trace(e)
                    obj.listener.onError(obj, name, e, log)
                    errorCount.incrementAndGet()
                    event(Status.Error, duration, \&/.This(e))
                  case NonFatal(e) =>
                    val duration = System.currentTimeMillis() - start
                    log.trace(e)
                    obj.listener.onError(obj, name, e, log)
                    errorCount.incrementAndGet()
                    event(Status.Error, duration, \&/.This(e))
                } finally {
                  cancel.set(true)
                  testCount.incrementAndGet()
                }
                eventHandler.handle(r)
                (check.prop, param, r)
              }
              case Maybe.Empty() =>
                LazyOption.lazyNone
            })
          }
          obj.listener.onFinishAll(obj, result, log)
          Array()
        } finally {
          executorService.shutdown()
        }
      }

      override def tags() = Array()
    }
  }

  override def tasks(taskDefs: Array[TaskDef]) = taskDefs.map(taskdef2task)

  override def done() =
    s"""done
Total test count: $testCount
Failed $failureCount, Errors $errorCount, Passed $successCount, Ignored $ignoredCount
"""

}
