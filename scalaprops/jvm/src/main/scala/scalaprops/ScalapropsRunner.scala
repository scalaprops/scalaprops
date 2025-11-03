package scalaprops

import sbt.testing.*
import scala.collection.mutable.ArrayBuffer
import scala.reflect.NameTransformer
import scalaprops.internal.*

object ScalapropsRunner {
  def testFieldNames(clazz: Class[?]): Array[String] =
    Scalaprops.testFieldNames(clazz)

  private[this] def invokeProperty(clazz: Class[?], obj: Scalaprops): List[(String, Property)] =
    Scalaprops
      .findTestFields(clazz, classOf[Property])
      .map { method =>
        val p = method.invoke(obj).asInstanceOf[Property]
        NameTransformer.decode(method.getName) -> p
      }
      .toList

  private[this] def invokeProperties(clazz: Class[?], obj: Scalaprops): List[Properties[Any]] =
    Scalaprops
      .findTestFields(clazz, classOf[Properties[?]])
      .map { method =>
        val methodName = NameTransformer.decode(method.getName)
        val props = method.invoke(obj).asInstanceOf[Properties[Any]].props
        Properties.noSort[Any](
          Tree.Node(
            methodName -> Option.empty,
            props #:: Stream.empty
          )
        )
      }
      .toList

  private[scalaprops] def allProps(
    clazz: Class[?],
    obj: Scalaprops,
    only: List[String],
    logger: Logger
  ): Properties[?] = {
    val tests0 = invokeProperty(clazz, obj).map { case (name, p) =>
      p.toProperties[Any](name)
    } ::: invokeProperties(clazz, obj)

    val tests = only match {
      case names @ (_ :: _) =>
        ScalapropsTaskImpl.filterTests(
          objName = clazz.getCanonicalName.dropRight(1),
          names = names,
          tests = tests0,
          logger = logger
        )
      case _ =>
        tests0
    }

    Properties.noSort[Any](
      Tree.Node(
        clazz.getName -> Option.empty,
        obj.transformProperties(tests).map(_.props).toStream
      )
    )
  }

  private[scalaprops] def getTestObject(
    fingerprint: Fingerprint,
    testClassName: String,
    testClassLoader: ClassLoader
  ): Scalaprops = {
    val clazz = testClassLoader.loadClass(testClassName + "$")
    clazz.getDeclaredField("MODULE$").get(null).asInstanceOf[Scalaprops]
  }

  private[scalaprops] def findTests(
    fingerprint: Fingerprint,
    testClassName: String,
    testClassLoader: ClassLoader,
    only: List[String],
    logger: Logger
  ): Properties[?] = {
    val clazz = testClassLoader.loadClass(testClassName + "$")
    val obj = getTestObject(fingerprint, testClassName, testClassLoader)
    allProps(clazz, obj, only, logger)
  }
}

final class ScalapropsRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader
) extends Runner {
  private[this] val status = TestStatus()

  private[this] val results = ArrayBuffer.empty[TestResult]
  private[this] val arguments = Arguments.parse(args.toList)

  override def tasks(taskDefs: Array[TaskDef]) =
    taskDefs.map { taskDef =>
      new ScalapropsTaskImpl(
        taskDef0 = taskDef,
        testClassLoader = testClassLoader,
        args = args,
        arguments = arguments,
        results = results,
        status = status
      )
    }

  override def done() = {
    s"""done
Total test count: ${status.all}
Failed ${status.failure}, Errors ${status.error}, Passed ${status.success}, Ignored ${status.ignored}
""" + TestResult.formatResults(results, arguments.showDuration)
  }
}
