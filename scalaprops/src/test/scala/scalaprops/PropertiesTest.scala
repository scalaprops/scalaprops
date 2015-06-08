package scalaprops

import scalaprops.Property.forAll
import scalaz._
import scalaz.std.string._

object PropertiesTest extends Scalaprops {

  // http://typelevel.org/blog/2013/11/17/discipline.html

  val ` "directed acyclic graph" to tree` = forAll {

    val functor = Tree.leaf("functor")
    val cobind = Tree.node("cobind", Stream(functor))
    val apply = Tree.node("apply", Stream(functor))
    val applicative = Tree.node("applicative", Stream(apply))
    val bind = Tree.node("bind", Stream(apply))
    val monad = Tree.node("monad", Stream(bind, applicative))
    val foldable = Tree.leaf("foldable")
    val traverse = Tree.node("traverse", Stream(foldable, functor))
    val plus = Tree.leaf("plus")
    val plusEmpty = Tree.node("plusEmpty", Stream(plus))
    val applicativePlus = Tree.node("applicativePlus", Stream( applicative, plusEmpty))
    val monadPlus = Tree.node("monadPlus", Stream(applicativePlus, monad))
    val isEmpty = Tree.node("isEmpty", Stream(plusEmpty))

    val list = Tree.node("list instance", Stream(
      isEmpty, monadPlus, traverse, cobind
    ))

    val dag = """"list instance"
|
+- "isEmpty"
|  |
|  `- "plusEmpty"
|     |
|     `- "plus"
|
+- "monadPlus"
|  |
|  +- "applicativePlus"
|  |  |
|  |  +- "applicative"
|  |  |  |
|  |  |  `- "apply"
|  |  |     |
|  |  |     `- "functor"
|  |  |
|  |  `- "plusEmpty"
|  |     |
|  |     `- "plus"
|  |
|  `- "monad"
|     |
|     +- "bind"
|     |  |
|     |  `- "apply"
|     |     |
|     |     `- "functor"
|     |
|     `- "applicative"
|        |
|        `- "apply"
|           |
|           `- "functor"
|
+- "traverse"
|  |
|  +- "foldable"
|  |
|  `- "functor"
|
`- "cobind"
   |
   `- "functor"
"""

    assert(list.drawTree == dag)

    val clazz = Properties.getClass
    val methodName = "distinctTree"
    val method = clazz.getDeclaredMethods.find(_.getName == methodName).getOrElse(sys.error("not found " + methodName))
    method.setAccessible(true)
    val tree = method.invoke(Properties, list, Order[String]).asInstanceOf[Tree[String]]
    tree.drawTree == """"list instance"
|
+- "isEmpty"
|  |
|  `- "plusEmpty"
|     |
|     `- "plus"
|
+- "monadPlus"
|  |
|  +- "applicativePlus"
|  |  |
|  |  `- "applicative"
|  |     |
|  |     `- "apply"
|  |        |
|  |        `- "functor"
|  |
|  `- "monad"
|     |
|     `- "bind"
|
+- "traverse"
|  |
|  `- "foldable"
|
`- "cobind"
"""
  }

}
