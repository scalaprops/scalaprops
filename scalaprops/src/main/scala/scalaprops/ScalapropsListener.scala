package scalaprops

import sbt.testing.Logger
import scalaz._

abstract class ScalapropsListener {

  def onStart(obj: Scalaprops, name: String, property: Property, param: Param, logger: Logger): Unit = {}

  def onFinish(obj: Scalaprops, name: String, property: Property, param: Param, result: CheckResult, logger: Logger): Unit = {}

  def onFinishAll(obj: Scalaprops, result: Tree[(Any, LazyOption[(Property, Param, ScalapropsEvent)])], logger: Logger): Unit = {}

  def onError(obj: Scalaprops, name: String, e: Throwable, logger: Logger): Unit = {}

  def onCheck(obj: Scalaprops, name: String, property: Property, param: Param, logger: Logger, count: Int): Unit = {}

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
        shift("+- ", "| ", drawTree(t)) ++ drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[(String, A)]): Stream[(String, A)] =
      (first #:: Stream.continually(other)).zip(s).map {
        case (a, (b, c)) =>
          (a + b, c)
      }

    ("" -> tree.rootLabel) #:: drawSubTrees(tree.subForest)
  }


  class Default extends ScalapropsListener {

    private[this] def event2string(event: ScalapropsEvent) =
      event.result match {
        case \&/.That(r) =>
          r.toString + " " + event.duration + "ms"
        case \&/.Both(_, r) =>
          r.toString + " " + event.duration + "ms"
        case \&/.This(e) =>
          e.toString + " " + event.duration + "ms"
      }

    override def onFinishAll(obj: Scalaprops, result: Tree[(Any, LazyOption[(Property, Param, ScalapropsEvent)])], logger: Logger): Unit = {
      val tree = drawTree(result.map {
        case (name, x) =>
          name.toString -> x.map{ case (prop, param, event) =>
            val str = event2string(event)
            if(logger.ansiCodesSupported()){
              event.result.b match {
                case Some(_: CheckResult.Proven | _: CheckResult.Passed) =>
                  Console.GREEN + str + Console.RESET
                case Some(_: CheckResult.Ignored) =>
                  Console.BLUE + str + Console.RESET
                case _ =>
                  Console.RED + str + Console.RESET
              }
            }else{
              str
            }
          }
      })
      println()
      val start = System.currentTimeMillis()
      // TODO ugly
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
          logger.error(s"exhausted ${Platform.className(obj.getClass)} $name $result")
        case _: CheckResult.Falsified =>
          logger.error(s"falsified ${Platform.className(obj.getClass)} $name $result")
        case _ =>
      }
    }

    override def onError(obj: Scalaprops, name: String, e: Throwable, logger: Logger): Unit = {
      logger.error(s"error ${Platform.className(obj.getClass)} $name")
      logger.trace(e)
      e.printStackTrace()
    }

    override def onCheck(obj: Scalaprops, name: String, property: Property, param: Param, logger: Logger, count: Int): Unit = {
      val N = 50
      val x = param.minSuccessful / N
      if((x <= 1) || (count % x == 0)){
        print(".")
      }
    }
  }
}
