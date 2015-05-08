package scalaprops

import sbt.testing.Logger
import scalaz._

abstract class ScalapropsListener {

  def onStart(obj: Scalaprops, name: String, property: Property, param: Param, logger: Logger): Unit = {}

  def onFinish(obj: Scalaprops, name: String, property: Property, param: Param, result: CheckResult, logger: Logger): Unit = {}

  def onFinishAll(obj: Scalaprops, result: Tree[(Any, LazyOption[(Property, Param, ScalapropsEvent)])], logger: Logger): Unit = {}

  def onError(obj: Scalaprops, name: String, e: Throwable, logger: Logger): Unit = {}

  def onCheck(obj: Scalaprops, name: String, check: Check, logger: Logger, count: Int): Unit = {}

}

object ScalapropsListener {
  val empty: ScalapropsListener = new ScalapropsListener {}

  val default: ScalapropsListener = new Default

  def drawTree[A](tree: Tree[A]): Stream[(String, A)] = {
    def drawSubTrees(s: Stream[Tree[A]]): Stream[(String, A)] = s match {
      case Stream.Empty =>
        Stream.Empty
      case Stream(t) =>
        shift("`- ", "  ", drawTree(t))
      case t #:: ts =>
        shift("+- ", "| ", drawTree(t)) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[(String, A)]): Stream[(String, A)] =
      (first #:: Stream.continually(other)).zip(s).map {
        case (a, (b, c)) =>
          (a + b, c)
      }

    ("" -> tree.rootLabel) #:: drawSubTrees(tree.subForest)
  }


  class Default extends ScalapropsListener {

    override def onFinishAll(obj: Scalaprops, result: Tree[(Any, LazyOption[(Property, Param, ScalapropsEvent)])], logger: Logger): Unit = {
      // TODO improve, more generalize
      def toShow(a: Any) = a match {
        case \/-(l: ScalazLaw) => l.simpleName
        case -\/(l: ScalazLaw) => l.simpleName
        case n : ScalazLaw => n.simpleName
        case \/-(id) => id
        case -\/(id) => id
        case _ => a.toString
      }

      val tree = drawTree(result.map {
        case (name, x) =>
          toShow(name) -> x.map{
            case (prop, param, event) =>
              event.result match {
                case \&/.That(r) =>
                  r.toString + " " + event.duration
                case \&/.Both(_, r) =>
                  r.toString + " " + event.duration
                case \&/.This(e) =>
                  e.toString + " " + event.duration
              }
          }
      })
      println()
      val start = System.currentTimeMillis()
      // TODO ugly. use scalaz-stream?
      tree.foreach{ case (treeLabel, (name, r)) =>
        print(treeLabel + name + " ")
        println(r.map(" " + _).getOrElse(""))
      }
      val name = tree.head._1 + tree.head._2._1
      logger.info(name + " " + (System.currentTimeMillis() - start) + " ms")
    }

    override def onFinish(obj: Scalaprops, name: String, property: Property, param: Param, result: CheckResult, logger: Logger): Unit = {
      result match {
        case e: CheckResult.GenException =>
          e.exception.printStackTrace()
          logger.trace(e.exception)
        case e: CheckResult.PropException =>
          e.exception.printStackTrace()
          logger.trace(e.exception)
        case _: CheckResult.Exhausted =>
          logger.error(s"exhausted ${obj.getClass.getCanonicalName} $name $result")
        case _: CheckResult.Falsified =>
          logger.error(s"falsified ${obj.getClass.getCanonicalName} $name $result")
        case _ =>
      }
    }

    override def onError(obj: Scalaprops, name: String, e: Throwable, logger: Logger): Unit = {
      logger.error(s"error ${obj.getClass.getCanonicalName} $name")
      logger.trace(e)
      e.printStackTrace()
    }

    override def onCheck(obj: Scalaprops, name: String, check: Check, logger: Logger, count: Int): Unit = {
      val param = check.paramEndo(obj.param)
      val N = 50
      val x = param.minSuccessful / N
      if((x <= 1) || (count % x == 0)){
        print(".")
      }
    }
  }
}
