package scalaprops

import java.lang.reflect.Method
import sbt.testing.Logger
import scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
import scalaprops.internal._

@EnableReflectiveInstantiation
trait Scalaprops {

  def param: Param = Param.withCurrentTimeSeed()

  def listener: ScalapropsListener =
    ScalapropsListener.default

  def transformProperties[A](properties: List[Properties[A]]): List[Properties[A]] =
    properties.map(Scalaprops.filterUnitEmpty).sortBy(_.id.toString)

}

object Scalaprops {

  def filterUnitEmpty[A](p: Properties[A]): Properties[A] = {
    def loop(tree: Tree[(A, Option[Check])]): Tree[(A, Option[Check])] =
      tree match {
        case Tree.Node(root, Stream(Tree.Node((Or.L(()), None), sub))) =>
          Tree.Node(root, sub.map(loop))
        case Tree.Node((root, None), Stream(Tree.Node(((), sub1), sub2))) =>
          Tree.Node(root -> sub1, sub2.map(loop))
        case _ =>
          Tree.Node(tree.rootLabel, tree.subForest.map(loop))
      }
    Properties.noSort(loop(p.props))
  }

  private[scalaprops] def testFieldNames(clazz: Class[_]): Array[String] =
    Array(
      findTestFields(clazz, classOf[Property]),
      findTestFields(clazz, classOf[Properties[_]])
    ).flatten.map(_.getName)

  private[scalaprops] def findTestFields(clazz: Class[_], fieldType: Class[_]): Array[Method] =
    clazz.getMethods.filter(method =>
      method.getParameterTypes.length == 0 && method.getReturnType == fieldType && !method.getName.contains("$anonfun")
    )

  private[scalaprops] def logger(loggers: Array[Logger]): Logger = new Logger {
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
