package scalaprops

import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.{TimeoutException, ForkJoinPool}
import java.util.concurrent.atomic.AtomicBoolean
import sbt.testing._
import scala.util.control.NonFatal
import scalaz._

object ScalapropsRunner {

  private[this] final val prefix = "test"

  private[this] def invokeProperty(clazz: Class[_], obj: Scalaprops): List[(String, Property)] =
    clazz.getMethods.filter(method =>
      method.getParameterTypes.length == 0 &&
      method.getName.startsWith(prefix) &&
      method.getReturnType == classOf[Property]
    ).map{ method =>
      val p = method.invoke(obj).asInstanceOf[Property]
      convertMethodName(method.getName.drop(prefix.length)) -> p
    }.toList

  private[this] def invokeProperties(clazz: Class[_], obj: Scalaprops): List[Properties[Any]] =
    clazz.getMethods.withFilter(method =>
      method.getParameterTypes.length == 0 &&
      method.getName.startsWith(prefix) &&
      method.getReturnType == classOf[Properties[_]]
    ).map{ method =>
      val methodName = convertMethodName(method.getName.drop(prefix.length))
      val props = method.invoke(obj).asInstanceOf[Properties[Any]].props
      Properties.noSort[Any](
        Tree.node(
          methodName -> Maybe.empty,
          props #:: Stream.empty
        )
      )
    }(collection.breakOut)

  private def allProps(clazz: Class[_], obj: Scalaprops): Properties[_] = {
    val tests = invokeProperty(clazz, obj).map {
      case (name, p) => p.toProperties[Any](name)
    } ::: invokeProperties(clazz, obj)

    Properties.noSort[Any](
      Tree.node(
        clazz.getName -> Maybe.empty,
        tests.map(_.props)(collection.breakOut)
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

  private[this] val EscapedUnicode = """\$u([0-9A-F]{4})""".r

  def convertMethodName(name: String): String = {
    val N = 6
    @annotation.tailrec
    def loop(i: Int, src: List[Char]): String =
      if(i <= (src.length - N)) {
        val (a, b) = src.splitAt(i)
        val (c, d) = b.splitAt(N)
        c.mkString match {
          case EscapedUnicode(u) =>
            loop(i + 1, a ::: Integer.decode("0x" + u).toChar :: d)
          case _ =>
            loop(i + 1, src)
        }
      }else{
        src.mkString
      }
    loop(0, name.toList)
  }
}

final class ScalapropsRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader
) extends Runner {

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

        try {
          val clazz = testClassLoader.loadClass(testClassName + "$")
          val obj = clazz.getDeclaredField("MODULE$").get(null).asInstanceOf[Scalaprops]
          val test = ScalapropsRunner.allProps(clazz, obj)
          val result = test.props.map { case (id, checkOpt) =>
            val name = id.toString // TODO create type class ?
            (id: Any) -> (checkOpt match{
              case Maybe.Just(check) => LazyOption.lazySome{
                val cancel = new AtomicBoolean(false)
                val selector = new TestSelector(name)
                def event(status: Status, e: OptionalThrowable, duration: Long) =
                  ScalapropsEvent(testClassName, taskdef.fingerprint(), selector, status, e, duration)

                val start = System.currentTimeMillis()
                val r = try {
                  obj.listener.onStart(obj, name, check, log)
                  val r = scalaz.concurrent.Task(
                    check.prop.check(
                      check.paramEndo(obj.param),
                      cancel,
                      count => obj.listener.onCheck(obj, name, check, log, count)
                    )
                  )(executorService).runFor(obj.param.timeout)
                  val duration = System.currentTimeMillis() - start
                  obj.listener.onFinish(obj, name, check, r, log)
                  val e = r match {
                    case _: CheckResult.Proven | _: CheckResult.Passed =>
                      event(Status.Success, emptyThrowable, duration)
                    case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
                      event(Status.Failure, emptyThrowable, duration)
                    case e: CheckResult.GenException =>
                      log.trace(e.exception)
                      event(Status.Error, new OptionalThrowable(e.exception), duration)
                    case e: CheckResult.PropException =>
                      log.trace(e.exception)
                      event(Status.Error, new OptionalThrowable(e.exception), duration)
                    case e: CheckResult.Timeout =>
                      event(Status.Error, emptyThrowable, duration)
                  }
                  e -> \/-((check.prop, r))
                } catch {
                  case e: TimeoutException =>
                    val duration = System.currentTimeMillis() - start
                    log.trace(e)
                    obj.listener.onError(obj, name, e, log)
                    event(Status.Canceled, new OptionalThrowable(e), duration) -> -\/(e)
                  case NonFatal(e) =>
                    val duration = System.currentTimeMillis() - start
                    log.trace(e)
                    obj.listener.onError(obj, name, e, log)
                    event(Status.Error, new OptionalThrowable(e), duration) -> -\/(e)
                } finally {
                  cancel.set(true)
                }
                eventHandler.handle(r._1)
                r._2
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

  override def done() = "done"

}
