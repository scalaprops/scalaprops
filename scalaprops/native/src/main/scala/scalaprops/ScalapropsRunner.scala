package scalaprops

import sbt.testing.*
import scala.collection.mutable.ArrayBuffer
import scala.scalanative.reflect.Reflect
import scalaprops.internal.Tree

object ScalapropsRunner {
  type EnableReflectiveInstantiation = scala.scalanative.reflect.annotation.EnableReflectiveInstantiation

  /** call from sbt plugin
    * [[https://github.com/scalaprops/sbt-scalaprops/blob/v0.2.5/src/main/scala/scalaprops/ScalapropsPlugin.scala#L66]]
    */
  def testFieldNames(clazz: Class[?]): Array[String] =
    Scalaprops.testFieldNames(clazz)

  private[scalaprops] def getTestObject(
    fingerprint: Fingerprint,
    testClassName: String,
    testClassLoader: ClassLoader
  ): Scalaprops = {
    Reflect
      .lookupLoadableModuleClass(testClassName + "$")
      .getOrElse(throw new ClassNotFoundException(testClassName + "$"))
      .loadModule()
      .asInstanceOf[Scalaprops]
  }

  private[scalaprops] def findTests(
    fingerprint: Fingerprint,
    testClassName: String,
    testClassLoader: ClassLoader,
    only: List[String],
    logger: Logger
  ): Properties[?] = {
    val obj = getTestObject(fingerprint, testClassName, testClassLoader)
    val tests0 = obj.property.getAll()

    val tests = only match {
      case names @ (_ :: _) =>
        ScalapropsTaskImpl.filterTests(
          objName = testClassName,
          names = names,
          tests = tests0,
          logger = logger
        )
      case _ =>
        tests0
    }

    Properties.noSort[Any](
      Tree.Node(
        testClassName -> Option.empty,
        obj.transformProperties(tests).map(_.props).toStream
      )
    )
  }
}

final class ScalapropsRunner(
  override val args: Array[String],
  remoteArgs0: Array[String],
  testClassLoader: ClassLoader
) extends Runner {

  override def remoteArgs(): Array[String] = remoteArgs0

  private[this] val results = ArrayBuffer.empty[TestResult]
  private[this] val arguments = Arguments.parse(args.toList)

  private[this] val taskdef2task: TaskDef => sbt.testing.Task = { taskdef =>
    new ScalapropsTaskImpl(taskdef, testClassLoader, args, arguments, results, TestStatus())
  }

  override def tasks(taskDefs: Array[TaskDef]) = taskDefs.map(taskdef2task)

  override def done() = {
    val result = TestResult.formatResults(results, arguments.showDuration)
    println(result)
    result
  }

  override def receiveMessage(msg: String): Option[String] = None

  override def serializeTask(task: sbt.testing.Task, serializer: TaskDef => String) =
    serializer(task.taskDef())

  override def deserializeTask(task: String, deserializer: String => TaskDef) =
    taskdef2task(deserializer(task))
}
