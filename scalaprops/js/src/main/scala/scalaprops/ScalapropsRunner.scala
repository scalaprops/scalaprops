package scalaprops

import org.scalajs.testinterface.TestUtils
import sbt.testing._
import scala.reflect.NameTransformer
import scala.scalajs._
import scala.scalajs.js.WrappedDictionary
import scalaz._
import scala.collection.mutable.ArrayBuffer

object ScalapropsRunner {

  /** call from sbt plugin
    * [[https://github.com/scalaprops/sbt-scalaprops/blob/v0.1.0/src/main/scala/scalaprops/ScalapropsPlugin.scala#L28]]
    */
  def testFieldNames(clazz: Class[_]): Array[String] =
    Scalaprops.testFieldNames(clazz)

  private[this] def findTestFields[A](obj: js.Dictionary[A], fieldType: Class[_]): WrappedDictionary[A] =
    obj.filter{ case (k, v) =>
      fieldType.isInstance(v)
    }.map{ case (k, v) =>
      val k0 = NameTransformer.decode(k)
      val k1 = k0.split('$').toSeq match {
        case init :+ _ => init.mkString("$")
        case _ => k0
      }
      k1 -> v
    }

  private[this] def invokeProperty[A](obj: js.Dictionary[A]): List[(String, Property)] =
    findTestFields(obj, classOf[Property]).map{ case (k, v) =>
      k -> v.asInstanceOf[Property]
    }.toList

  private[this] def invokeProperties[A](obj: js.Dictionary[A]): List[Properties[Any]] =
    findTestFields(obj, classOf[Properties[_]]).map{ case (name, properties) =>
      val props = properties.asInstanceOf[Properties[Any]].props
      Properties.noSort[Any](
        Tree.Node(
          name -> Maybe.empty,
          props #:: Stream.empty
        )
      )
    }(collection.breakOut)

  def allProps(obj: Scalaprops, only: Option[NonEmptyList[String]], logger: Logger): Properties[_] = {
    val tests0 = invokeProperty(obj.asInstanceOf[js.Dictionary[_]]).map {
      case (name, p) => p.toProperties[Any](name)
    } ::: invokeProperties(obj.asInstanceOf[js.Dictionary[_]])

    val tests = only match {
      case Some(names) =>
        val set = Foldable[NonEmptyList].toSet(names)
        val actualTests: Set[String] = tests0.map(_.id.toString)(collection.breakOut)
        set.filterNot(actualTests).foreach{ typo =>
          logger.warn(s"""'${obj.toString.dropRight(1)}.$typo' does not exists""")
        }
        tests0.filter(p => set(p.id.toString))
      case None =>
        tests0
    }

    Properties.noSort[Any](
      Tree.Node(
        obj.getClass.getName -> Maybe.empty,
        obj.transformProperties(tests).map(_.props)(collection.breakOut)
      )
    )
  }

  private[scalaprops] def getTestObject(
    fingerprint: Fingerprint,
    testClassName: String,
    testClassLoader: ClassLoader
  ): Scalaprops = {
    fingerprint match {
      case f: SubclassFingerprint if f.superclassName() == "scalaprops.Scalaprops" =>
        if (f.isModule) {
          TestUtils.loadModule(testClassName, testClassLoader) match {
            case m: Scalaprops => m
            case x => throw new Exception(s"Cannot test $testClassName of type: $x")
          }
        } else {
          throw new Exception("Scalaprops only works on objects, classes don't work.")
        }
      case _ => throw new Exception("can not find scalaporps.Scalaprops instance.")
    }
  }

  private[scalaprops] def findTests(
    fingerprint: Fingerprint,
    testClassName: String,
    testClassLoader: ClassLoader,
    only: Option[NonEmptyList[String]],
    logger: Logger
  ): Properties[_] = {
    val obj = getTestObject(fingerprint, testClassName, testClassLoader)
    allProps(obj, only, logger)
  }
}

final class ScalapropsRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader
) extends Runner {

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

  override def receiveMessage(msg: String) = None

  override def serializeTask(task: sbt.testing.Task, serializer: TaskDef => String) =
    serializer(task.taskDef())

  override def deserializeTask(task: String, deserializer: String => TaskDef) =
    taskdef2task(deserializer(task))
}
