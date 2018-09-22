package scalaprops

import sbt.testing.{Event, EventHandler, Logger, Status}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.NameTransformer
import scalaprops.internal._

object NativeTestHelper {

  private val logger: Logger = new Logger {
    override def debug(msg: String): Unit =
      println(msg)
    override def error(msg: String): Unit = {}
    override val ansiCodesSupported: Boolean =
      true
    override def warn(msg: String): Unit =
      println(msg)
    override def trace(t: Throwable): Unit = {}
    override def info(msg: String): Unit =
      println(msg)
  }

}

abstract class NativeTestHelper {
  import NativeTestHelper._

  private[this] val events = new ArrayBuffer[Event]

  private[this] val eventHandler = new EventHandler {
    override def handle(event: Event) = synchronized{
      events += event
    }
  }

  protected[this] final def convert(p: Property): Properties[Unit] =
    Properties.single((), p)

  protected[this] final def convert[A](p: Properties[A]): Properties[A] =
    p

  private[this] val results = ArrayBuffer.empty[TestResult]
  private[this] val status = TestStatus()

  protected[this] def test(className: String, obj: Scalaprops, objects: Set[String], args: Arguments, props: (String, Properties[_])*): Unit = {
    val testsOpt = {
      if(objects.isEmpty || objects.contains(className)) {
        val tests = props.map{ case (id, p) =>
          Properties(Tree.Node(
            NameTransformer.decode(id) -> Option.empty[Check],
            Stream(p.widen[Any].props)
          ))
        }.toList

        args.only match {
          case names @ (_ :: _) =>
            // test specific fields
            Some(
              ScalapropsTaskImpl.filterTests(
                objName = obj.toString.dropRight(1),
                names = names,
                tests = tests,
                logger = logger
              )
            )
          case Nil =>
            // test all fields
            Some(tests)
        }
      } else {
        // ignore this object test
        None
      }
    }

    testsOpt.foreach { tests =>
      val tree = ScalapropsTaskImpl.createTree(
        tests = Properties(Tree.Node(
          className -> Option.empty[Check],
          obj.transformProperties(tests).toStream.map(_.props)
        )),
        testClassName = className,
        arguments = args,
        results = results,
        testStatus = status,
        eventHandler = eventHandler,
        log = logger,
        obj = obj,
        fingerprint = ScalapropsFingerprint,
        executor = TestExecutorImpl.instance
      )
      obj.listener.onFinishAll(obj, tree, logger)
    }
  }

  protected[this] def finish(count: Int): Unit = {
    println(resultString(count))
    println()
    events.toList.withFilter{ e =>
      val s = e.status()
      Status.Error == s || Status.Failure == s
    }.foreach{ e =>
      println(Console.RED + e.fullyQualifiedName() + Console.RESET)
    }
    println()
    if(status.failure.get + status.error.get > 0) sys.error("test failed")
  }

  protected[this] def resultString(count: Int): String = {
    s"""
done
Total test count: ${status.all}
Failed ${status.failure}, Errors ${status.error}, Passed ${status.success}, Ignored ${status.ignored}

""" + TestResult.formatResults(results, count)
  }

  protected[this] def maybeNativeEnvironment(): Boolean = {
    (new Throwable).getStackTrace.forall{ s =>
      (s.getFileName == null) && (s.getLineNumber == 0)
    }
  }

  private[this] val notNativeEnvMessage = "Maybe this is not scala-native environment!"

  protected[this] def warnIfNotNativeEnvironment(): Unit = {
    if(maybeNativeEnvironment == false) {
      Console.err.println(Console.RED + notNativeEnvMessage + Console.RESET)
    }
  }

  protected[this] def throwIfNotNativeEnvironment(): Unit = {
    if(maybeNativeEnvironment == false) {
      sys.error(notNativeEnvMessage)
    }
  }
}
